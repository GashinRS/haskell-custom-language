{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Interpreter
where

import Parser

evalPrint :: PrintExp -> IO()
evalPrint (Print i) =  print $ evalIntExp i

evalStatements :: [Statement] -> IO()
evalStatements [x]    = evalStatement x
evalStatements (x:xs) = evalStatement x  >> evalStatements xs

evalStatement :: Statement -> IO()
evalStatement (PrintStm exp) = evalPrint exp
evalStatement (IfStm exp)    = evalIfExp exp
evalStatement (VarStm exp)   = evalVarExp exp
evalStatement (WhileStm exp) = evalWhileExp exp

evalVarExp :: VariableExp -> IO()
evalVarExp (IntVar i)  = return()
evalVarExp (BoolVar i) = return() 

evalWhileExp = undefined

evalIfExp :: IfExp -> IO()
evalIfExp (If b e)
                | evalBoolExp b       = evalStatements e
                | not $ evalBoolExp b = return()

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