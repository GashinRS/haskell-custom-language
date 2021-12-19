{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
where

import Data.List
import Data.Maybe
import qualified Data.Bifunctor
import Parser
import qualified Data.Map as Map

data Evaluation = Eval { variables :: Map.Map String Int,
                        functions :: Map.Map String ([IntExp], [Statement]),
                        io :: IO()
                        }

evalStatements :: [Statement] -> Evaluation -> Evaluation
evalStatements [] es     = es
evalStatements [x] es    = evalStatement x es
evalStatements (x:xs) es = Eval v' f' $ i >> i'
                                where (Eval v f i)    = evalStatement x es
                                      (Eval v' f' i') = evalStatements xs $ Eval v f i

evalStatement :: Statement -> Evaluation -> Evaluation
evalStatement (PrintStm exp) es    = evalPrint exp es
evalStatement (IfStm exp) es       = evalIfExp exp es
evalStatement (WhileStm exp) es    = evalWhileExp exp (return()) es
evalStatement (VarStm exp) es      = evalVarExp exp es
evalStatement (FunctionStm exp) es = evalFunction exp es

evalPrint :: PrintExp -> Evaluation -> Evaluation
evalPrint (PrintInt p) (Eval v f i) = Eval v f $ print (evalIntExp p (Eval v f i))
evalPrint (PrintVar p) (Eval v f i)
                        | isJust lu    = Eval v f $ print (fromJust lu)
                        | isNothing lu = Eval v f i
                            where lu = Map.lookup p v

evalIfExp :: IfExp -> Evaluation -> Evaluation
evalIfExp (If b s) (Eval v f i)
                | evalBoolExp b (Eval v f i)       = evalStatements s (Eval v f i)
                | not $ evalBoolExp b (Eval v f i) = Eval v f $ return()

evalVarExp :: VariableExp -> Evaluation -> Evaluation
evalVarExp (Var (s, IntLit il)) (Eval v f i) = Eval (Map.insert s il v) f i
evalVarExp (Var (s, VarLit s')) (Eval v f i)
                            | isNothing lu = error $ "variable " ++ s' ++ " does not exist"
                            | isJust lu    = Eval (Map.insert s (fromJust lu) v) f i
                                where lu = Map.lookup s' v
evalVarExp (Var (s, intExp)) (Eval v f i)    = Eval (Map.insert s (evalIntExp intExp (Eval v f i)) v) f i

evalFunction :: FunctionExp -> Evaluation -> Evaluation
--function call
evalFunction (Function name args []) (Eval v f i) 
                                            | isNothing lu = error $ "function " ++ name ++ " does not exist"
                                            | isJust lu    = evalStatements (snd $ fromJust lu) (Eval v f i)
                                                where lu = Map.lookup name f
--function declaration
evalFunction (Function name args stms) (Eval v f i) = Eval v (Map.insert name (args, stms) f) i 

evalWhileExp :: WhileExp -> IO() -> Evaluation -> Evaluation
evalWhileExp (While b e) io (Eval v f i)
                    | evalBoolExp b (Eval v f i)       = evalWhileExp (While b e) (io >> i') $ Eval v' f' $ return()
                    | not $ evalBoolExp b (Eval v f i) = Eval v f io
                    where (Eval v' f' i') = evalStatements e (Eval v f i) --next loop

evalBoolExp :: BoolExp -> Evaluation -> Bool
evalBoolExp (BoolLit b) vs = b
evalBoolExp (e :==: e') (Eval v f i) = evalIntExp e (Eval v f i) == evalIntExp e' (Eval v f i)
evalBoolExp (e :/=: e') (Eval v f i) = evalIntExp e (Eval v f i) /= evalIntExp e' (Eval v f i)
evalBoolExp (e :<: e')  (Eval v f i) = evalIntExp e (Eval v f i) < evalIntExp e' (Eval v f i)
evalBoolExp (e :<=: e') (Eval v f i) = evalIntExp e (Eval v f i) <= evalIntExp e' (Eval v f i)
evalBoolExp (e :>: e')  (Eval v f i) = evalIntExp e (Eval v f i) > evalIntExp e' (Eval v f i)
evalBoolExp (e :>=: e') (Eval v f i) = evalIntExp e (Eval v f i) >= evalIntExp e' (Eval v f i)
evalBoolExp (e :&&: e') (Eval v f i) = evalBoolExp e (Eval v f i) && evalBoolExp e' (Eval v f i)
evalBoolExp (e :||: e') (Eval v f i) = evalBoolExp e (Eval v f i) || evalBoolExp e' (Eval v f i)

evalIntExp :: IntExp -> Evaluation -> Int
evalIntExp (IntLit n) (Eval v f i) = n
evalIntExp (VarLit vl) (Eval v f i)
                    | isNothing lu = error $ "variable " ++ vl ++ " does not exist"
                    | isJust lu    = fromJust lu
                        where lu = Map.lookup vl v
evalIntExp (e :+: e') (Eval v f i) = evalIntExp e (Eval v f i) + evalIntExp e' (Eval v f i)
evalIntExp (e :-: e') (Eval v f i) = evalIntExp e (Eval v f i) - evalIntExp e' (Eval v f i)
evalIntExp (e :*: e') (Eval v f i) = evalIntExp e (Eval v f i) * evalIntExp e' (Eval v f i)
evalIntExp (e :/: e') (Eval v f i) = evalIntExp e (Eval v f i) `div` evalIntExp e' (Eval v f i)