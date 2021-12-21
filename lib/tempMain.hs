module TempMain
where

import qualified Data.Map as Map
import Graphics.Gloss
import System.Random (StdGen, getStdGen, randomR)
import Graphics.Gloss.Interface.Pure.Game
import Interpreter
import Engine
import Parser

callGameFunction :: String -> Evaluation -> Evaluation
callGameFunction name = evalVoidFunctionCall (FunctionCall name [])

startGame :: (Int, StdGen) -> Evaluation -> Evaluation
startGame r (Eval v f i (BlockGame s p d t)) = Eval v f i (BlockGame s p north t)

gamePic :: Evaluation -> Picture
gamePic (Eval v f i (BlockGame s p d t)) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], Pictures[drawCoord x | x <- t]]

move :: Event -> Evaluation -> Evaluation
move (EventKey (SpecialKey KeyLeft) Down _ _) e  = callGameFunction "moveLeft" e
move (EventKey (SpecialKey KeyRight) Down _ _) e = callGameFunction "moveRight" e
move (EventKey (SpecialKey KeyDown) Down _ _) e  = callGameFunction "moveDown" e
move (EventKey (SpecialKey KeyUp) Down _ _) e    = callGameFunction "moveUp" e
move _ g                                         = g


next :: Float -> Evaluation -> Evaluation
next x = callGameFunction "nextStep"
