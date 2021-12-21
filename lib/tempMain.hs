
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
startGame r (Eval v f i (BlockGame s p d t b)) = Eval v f i (BlockGame s p north t b)
startGame _ e                                  = e

gamePic :: Evaluation -> Picture
gamePic (Eval v f i (BlockGame s p d t b)) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], 
                                            Pictures[drawCoord x | x <- t], Pictures[drawCoord x | x <- b]]
gamePic (Eval v f i Won)                   = displayMessage "You won!"
gamePic (Eval v f i GameOver)              = displayMessage "You lost!"

move :: Event -> Evaluation -> Evaluation
move (EventKey (SpecialKey KeyLeft) Down _ _) e  = callGameFunction "moveLeft" e
move (EventKey (SpecialKey KeyRight) Down _ _) e = callGameFunction "moveRight" e
move (EventKey (SpecialKey KeyDown) Down _ _) e  = callGameFunction "moveDown" e
move (EventKey (SpecialKey KeyUp) Down _ _) e    = callGameFunction "moveUp" e
move (EventKey (SpecialKey KeySpace) Down _ _) e = callGameFunction "pressSpace" e
move _ g                                         = g


next :: Float -> Evaluation -> Evaluation
next x (Eval v f i (BlockGame s p d t b)) = callGameFunction "nextStep" (Eval v f i (BlockGame s p d t b))
next x e                                  = e
