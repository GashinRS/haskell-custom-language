{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
where

import Data.List
import Data.Maybe
import qualified Data.Bifunctor
import Parser
import qualified Data.Map as Map
import Engine

data Evaluation = Eval { variables :: Map.Map String Int,
                         functions :: Map.Map String FunctionValues,
                         io :: IO(),
                         game :: Game
                        }

data FunctionValues = FunV { arguments :: [String],
                             body :: [Statement],
                             returnValue :: IntExp
                            }

evalStatements :: [Statement] -> Evaluation -> Evaluation
evalStatements [] es     = es
evalStatements [x] es    = evalStatement x es
evalStatements (x:xs) es = Eval v' f' (i >> i') g'
                                where (Eval v f i g)     = evalStatement x es
                                      (Eval v' f' i' g') = evalStatements xs $ Eval v f i g

evalStatement :: Statement -> Evaluation -> Evaluation
evalStatement (PrintStm exp) es        = evalPrint exp es
evalStatement (IfStm exp) es           = evalIfExp exp es
evalStatement (WhileStm exp) es        = evalWhileExp exp (return()) es
evalStatement (VarStm exp) es          = evalVarExp exp es
evalStatement (FunctionDeclStm exp) es = evalFunctionDecl exp es
evalStatement (FunctionCallStm exp) es = evalVoidFunctionCall exp es

evalPrint :: PrintExp -> Evaluation -> Evaluation
evalPrint (PrintInt p) (Eval v f i g) = Eval v f (print $ evalIntExp p $ Eval v f i g) g
evalPrint (PrintVar p) (Eval v f i g)
                        | isJust lu = Eval v f (print $ fromJust lu) g
                        | otherwise = Eval v f i g
                            where lu = Map.lookup p v

evalIfExp :: IfExp -> Evaluation -> Evaluation
evalIfExp (If b s) (Eval v f i g)
                | evalBoolExp b (Eval v f i g) = evalStatements s (Eval v f i g)
                | otherwise                    = Eval v f (return()) g

evalVarExp :: VariableExp -> Evaluation -> Evaluation
evalVarExp (Var (s, IntLit il)) (Eval v f i g) = Eval (Map.insert s il v) f i g
evalVarExp (Var (s, VarLit s')) (Eval v f i g)
                            | isNothing lu = error $ "variable " ++ s' ++ " does not exist"
                            | otherwise    = Eval (Map.insert s (fromJust lu) v) f i g
                                where lu = Map.lookup s' v
evalVarExp (Var (s, FunctionCallLit name arguments)) (Eval v f i g) = Eval (Map.insert s (evalIntExp (FunctionCallLit name arguments) (Eval v f i g)) v) f i g
evalVarExp (Var (s, intExp)) (Eval v f i g)                         = Eval (Map.insert s (evalIntExp intExp (Eval v f i g)) v) f i g

evalFunctionDecl :: FunctionDeclExp -> Evaluation -> Evaluation
evalFunctionDecl (FunctionDecl name args stms returnV) (Eval v f i g) = Eval v (Map.insert name (FunV args stms returnV) f) i g

--wordt gebruikt om built in getters op te roepen
evalGetterCall :: String -> [Int] -> Game -> Int 
evalGetterCall name args game
                | isNothing lu = error $ "function " ++ name ++ " does not exist"
                | otherwise    = fromJust lu args game
                where lu = Map.lookup name getters

evalFunctionCall :: FunctionCallExp -> Evaluation -> Int
evalFunctionCall (FunctionCall name args) e = evalIntExp (returnValue $ fromJust $ Map.lookup name f) $ Eval v f i g
                                                            where (Eval v f i g) = evalVoidFunctionCall (FunctionCall name args) e

--wordt gebruikt om te kijken of built in void functies opgeroepen worden, dit wordt uitgevoerd voor evalVoidFunctionCall'
evalVoidFunctionCall :: FunctionCallExp -> Evaluation -> Evaluation
evalVoidFunctionCall (FunctionCall name args) (Eval v f i g)
                                                    | isNothing builtIn = evalVoidFunctionCall' (FunctionCall name args) (Eval v f i g)
                                                    | otherwise         = Eval v f i $ fromJust builtIn (intExpListToIntList args (Eval v f i g)) g
                                            where builtIn = Map.lookup name builtInFunctions 

--wordt gebruikt om te kijken of user defined functies worden opgeroepen
evalVoidFunctionCall' :: FunctionCallExp -> Evaluation -> Evaluation
evalVoidFunctionCall' (FunctionCall name args) (Eval v f i g)                                                   
                                                    | let newVar = insertListInMap (arguments $ fromJust lu) (intExpListToIntList args (Eval v f i g)) v, 
                                                        isJust lu = evalStatements (body $ fromJust lu) (Eval newVar f i g)
                                                    | otherwise  = error $ "function " ++ name ++ " does not exist"
                                            where lu = Map.lookup name f

intExpListToIntList :: [IntExp] -> Evaluation -> [Int]
intExpListToIntList intExps e = [evalIntExp x e | x <- intExps]

--wordt gebruikt om een lijst van variabelen en hun respectievelijke waarden in de map te steken met alle variabelen
insertListInMap :: [String] -> [Int] -> Map.Map String Int -> Map.Map String Int
insertListInMap [] [] m         = m
insertListInMap (s:ss) (i:is) m = insertListInMap ss is (Map.insert s i m)

evalWhileExp :: WhileExp -> IO() -> Evaluation -> Evaluation
evalWhileExp (While b e) io (Eval v f i g)
                    | evalBoolExp b (Eval v f i g)       = evalWhileExp (While b e) (io >> i') $ Eval v' f' (return()) g'
                    | otherwise                          = Eval v f io g
                    where (Eval v' f' i' g') = evalStatements e (Eval v f i g) --next loop

evalBoolExp :: BoolExp -> Evaluation -> Bool
evalBoolExp (BoolLit b) vs = b
evalBoolExp (e :==: e') (Eval v f i g) = evalIntExp e (Eval v f i g) == evalIntExp e' (Eval v f i g)
evalBoolExp (e :/=: e') (Eval v f i g) = evalIntExp e (Eval v f i g) /= evalIntExp e' (Eval v f i g)
evalBoolExp (e :<: e')  (Eval v f i g) = evalIntExp e (Eval v f i g) < evalIntExp e' (Eval v f i g)
evalBoolExp (e :<=: e') (Eval v f i g) = evalIntExp e (Eval v f i g) <= evalIntExp e' (Eval v f i g)
evalBoolExp (e :>: e')  (Eval v f i g) = evalIntExp e (Eval v f i g) > evalIntExp e' (Eval v f i g)
evalBoolExp (e :>=: e') (Eval v f i g) = evalIntExp e (Eval v f i g) >= evalIntExp e' (Eval v f i g)
evalBoolExp (e :&&: e') (Eval v f i g) = evalBoolExp e (Eval v f i g) && evalBoolExp e' (Eval v f i g)
evalBoolExp (e :||: e') (Eval v f i g) = evalBoolExp e (Eval v f i g) || evalBoolExp e' (Eval v f i g)

evalIntExp :: IntExp -> Evaluation -> Int
evalIntExp (IntLit n) (Eval v f i g) = n
evalIntExp (VarLit vl) (Eval v f i g)
                    | isNothing lu   = error $ "variable " ++ vl ++ " does not exist"
                    | vl == stripped = fromJust lu
                    | otherwise      = fromJust lu * (-1)
                        where stripped = stripMinus vl
                              lu = Map.lookup stripped v
evalIntExp (FunctionCallLit name arguments) (Eval v f i g)
                                                    | isNothing lu = evalGetterCall name (intExpListToIntList arguments (Eval v f i g)) g
                                                    | otherwise    = evalFunctionCall (FunctionCall name arguments) $ Eval v f i g
                                                        where lu = Map.lookup name f
evalIntExp (e :+: e') (Eval v f i g) = evalIntExp e (Eval v f i g) + evalIntExp e' (Eval v f i g)
evalIntExp (e :-: e') (Eval v f i g) = evalIntExp e (Eval v f i g) - evalIntExp e' (Eval v f i g)
evalIntExp (e :*: e') (Eval v f i g) = evalIntExp e (Eval v f i g) * evalIntExp e' (Eval v f i g)
evalIntExp (e :/: e') (Eval v f i g) = evalIntExp e (Eval v f i g) `div` evalIntExp e' (Eval v f i g)

stripMinus :: String -> String 
stripMinus (s:ss) 
                |s == '-'  = ss
                |otherwise = s:ss