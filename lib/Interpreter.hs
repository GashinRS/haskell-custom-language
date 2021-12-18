{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
where

import Data.List
import Data.Maybe
import qualified Data.Bifunctor
import Parser
import Data.Map

evalPrint :: PrintExp -> [VariableExp] -> ([VariableExp], IO())
evalPrint (PrintInt i) vs  =  (vs, print $ evalIntExp i)
evalPrint (PrintVar i) vs
                        | isJust f    = (vs, print $ fromJust f)
                        | isNothing f = (vs, return())
                            where f = findVar i vs

evalStatements :: [Statement] -> [VariableExp] -> ([VariableExp], IO())
evalStatements [x] vs    = evalStatement x vs
evalStatements (x:xs) vs = Data.Bifunctor.bimap (fst e ++) (snd e >>) es
                                where e  = evalStatement x vs
                                      es = evalStatements xs $ fst e

evalStatement :: Statement ->  [VariableExp] -> ([VariableExp], IO())
evalStatement (PrintStm exp) vs = evalPrint exp vs
evalStatement (IfStm exp) vs    = evalIfExp exp vs
evalStatement (VarStm exp) vs   = evalVarExp exp vs
-- evalStatement (WhileStm exp) vs = evalWhileExp exp 

evalVarExp :: VariableExp -> [VariableExp] -> ([VariableExp], IO())
evalVarExp v vs  = (v:vs, return())

runVar :: VariableExp -> (String, Int)
runVar (IntVar i) = i

findVar :: String -> [VariableExp] -> Maybe Int
findVar s [] = Nothing
findVar s (v:vs)
            | fst rv == s = Just $ snd rv
            | fst rv /= s = findVar s vs
                where rv = runVar v

evalWhileExp = undefined

evalIfExp :: IfExp -> [VariableExp] -> ([VariableExp], IO())
evalIfExp (If b e) vs
                | evalBoolExp b       = evalStatements e vs
                | not $ evalBoolExp b = (vs, return())

evalBoolExp :: BoolExp -> Bool
evalBoolExp (BoolLit b) = b
evalBoolExp (e :==: f)  = evalIntExp e == evalIntExp f
evalBoolExp (e :/=: f)  = evalIntExp e /= evalIntExp f
evalBoolExp (e :<: f)   = evalIntExp e < evalIntExp f
evalBoolExp (e :<=: f)  = evalIntExp e <= evalIntExp f
evalBoolExp (e :>: f)   = evalIntExp e > evalIntExp f
evalBoolExp (e :>=: f)  = evalIntExp e >= evalIntExp f
evalBoolExp (e :&&: f)  = evalBoolExp e && evalBoolExp f
evalBoolExp (e :||: f)  = evalBoolExp e || evalBoolExp f

evalIntExp :: IntExp -> Int
evalIntExp (IntLit n) = n
evalIntExp (e :+: f)  = evalIntExp e + evalIntExp f
evalIntExp (e :-: f)  = evalIntExp e - evalIntExp f
evalIntExp (e :*: f)  = evalIntExp e * evalIntExp f
evalIntExp (e :/: f)  = evalIntExp e `div` evalIntExp f