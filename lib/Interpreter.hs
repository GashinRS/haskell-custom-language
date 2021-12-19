{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
where

import Data.List
import Data.Maybe
import qualified Data.Bifunctor
import Parser
import qualified Data.Map as Map

evalStatements :: [Statement] -> Map.Map String Int -> (Map.Map String Int, IO())
evalStatements [x] vs    = evalStatement x vs
evalStatements (x:xs) vs = Data.Bifunctor.bimap (Map.union (fst e)) (snd e >>) es
                                where e  = evalStatement x vs
                                      es = evalStatements xs $ fst e

evalStatement :: Statement -> Map.Map String Int -> (Map.Map String Int, IO())
evalStatement (PrintStm exp) vs = evalPrint exp vs
evalStatement (IfStm exp) vs    = evalIfExp exp vs
evalStatement (WhileStm exp) vs = evalWhileExp exp (return()) vs
evalStatement (VarStm exp) vs   = evalVarExp exp vs

evalPrint :: PrintExp -> Map.Map String Int -> (Map.Map String Int, IO())
evalPrint (PrintInt i) vs  =  (vs, print $ evalIntExp i vs)
evalPrint (PrintVar i) vs
                        | isJust f    = (vs, print $ fromJust f)
                        | isNothing f = (vs, return())
                            where f = Map.lookup i vs

evalIfExp :: IfExp -> Map.Map String Int -> (Map.Map String Int, IO())
evalIfExp (If b e) vs
                | evalBoolExp b vs       = evalStatements e vs
                | not $ evalBoolExp b vs = (vs, return())

evalVarExp :: VariableExp -> Map.Map String Int -> (Map.Map String Int, IO())
evalVarExp (Var (s, IntLit i)) vs = (uncurry Map.insert (s, i) vs, return())
evalVarExp (Var (s, VarLit s')) vs
                            | isNothing lu = (vs, return())
                            | isJust lu    = (Map.insert s (fromJust lu) vs, return())
                                where lu = Map.lookup s' vs
evalVarExp (Var (s, i)) vs        = (Map.insert s (evalIntExp i vs) vs, return())

evalWhileExp :: WhileExp -> IO() -> Map.Map String Int -> (Map.Map String Int, IO())
evalWhileExp (While b e) io vs
                    | evalBoolExp b vs       = evalWhileExp (While b e) (io >> snd nextLoop) $ fst nextLoop
                    | not $ evalBoolExp b vs = (vs, io)
                    where nextLoop = evalStatements e vs

evalBoolExp :: BoolExp -> Map.Map String Int -> Bool
evalBoolExp (BoolLit b) vs = b
evalBoolExp (e :==: f)  vs = evalIntExp e vs == evalIntExp f vs
evalBoolExp (e :/=: f)  vs = evalIntExp e vs /= evalIntExp f vs
evalBoolExp (e :<: f)   vs = evalIntExp e vs < evalIntExp f vs
evalBoolExp (e :<=: f)  vs = evalIntExp e vs <= evalIntExp f vs
evalBoolExp (e :>: f)   vs = evalIntExp e vs > evalIntExp f vs
evalBoolExp (e :>=: f)  vs = evalIntExp e vs >= evalIntExp f vs
evalBoolExp (e :&&: f)  vs = evalBoolExp e vs && evalBoolExp f vs
evalBoolExp (e :||: f)  vs = evalBoolExp e vs || evalBoolExp f vs

evalIntExp :: IntExp -> Map.Map String Int -> Int
evalIntExp (IntLit n) vs = n
evalIntExp (VarLit v) vs
                    | isNothing lu = error $ "variable " ++ v ++ " does not exist"
                    | isJust lu    = fromJust lu
                        where lu = Map.lookup v vs
evalIntExp (e :+: f)  vs = evalIntExp e vs + evalIntExp f vs
evalIntExp (e :-: f)  vs = evalIntExp e vs - evalIntExp f vs
evalIntExp (e :*: f)  vs = evalIntExp e vs * evalIntExp f vs
evalIntExp (e :/: f)  vs = evalIntExp e vs `div` evalIntExp f vs