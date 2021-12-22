import Parser
import Data.Char
import Interpreter
import qualified Data.Map as Map
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import System.Environment (getArgs)


callGameFunction :: String -> Evaluation -> Evaluation
callGameFunction name = evalVoidFunctionCall (FunctionCall name [])

startGame :: (Int, StdGen) -> Evaluation -> Evaluation
startGame random (Eval v f i (BlockGame p d t b r)) = Eval v f i (BlockGame p d t b random)
startGame _ e                                       = e

gamePic :: Evaluation -> Picture
gamePic (Eval v f i (BlockGame p d t b r)) = Pictures[emptyBoard, Pictures[drawCoord x | x <- p], 
                                            Pictures[drawCoord x | x <- t], Pictures[drawCoord x | x <- b]]
gamePic (Eval v f i Won)                   = displayMessage "You won!"
gamePic (Eval v f i GameOver)              = displayMessage "You lost!"

move :: Event -> Evaluation -> Evaluation
move (EventKey (SpecialKey KeyLeft) Down _ _) (Eval v f i (BlockGame p d t b r))  = callGameFunction "moveLeft" (Eval v f i (BlockGame p d t b r))
move (EventKey (SpecialKey KeyRight) Down _ _) (Eval v f i (BlockGame p d t b r)) = callGameFunction "moveRight" (Eval v f i (BlockGame p d t b r))
move (EventKey (SpecialKey KeyDown) Down _ _) (Eval v f i (BlockGame p d t b r))  = callGameFunction "moveDown" (Eval v f i (BlockGame p d t b r))
move (EventKey (SpecialKey KeyUp) Down _ _) (Eval v f i (BlockGame p d t b r))    = callGameFunction "moveUp" (Eval v f i (BlockGame p d t b r))
move (EventKey (SpecialKey KeySpace) Down _ _) (Eval v f i (BlockGame p d t b r)) = callGameFunction "pressSpace" (Eval v f i (BlockGame p d t b r))
move _ g                                                                          = g


next :: Float -> Evaluation -> Evaluation
next x (Eval v f i (BlockGame p d t b r)) = callGameFunction "nextStep" (Eval v f i (BlockGame p d t b r))
next x e                                  = e

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          stdGen <- getStdGen
          let r = getRandomNumberInRange stdGen 0 42
          let all = evalStatements (parse parseStatement $ filter (not . isSpace) contents) $ Eval Map.empty Map.empty (return()) $ BlockGame [] (0,0) [] [] r
          let funs = functions all
          let start = callGameFunction "startGame" all
          play (InWindow "UGent Brick Game" (500, 800) (10, 10))
                    screenGreen -- de achtergrondkleur
                    2 -- aantal stappen per seconde
                    (startGame r start)-- de beginwereld
                    gamePic -- de 'render'-functie, om naar scherm te tekenen
                    move -- de 'handle'-functie, om gebruiksinvoer te verwerken
                    next -- de 'step'-functie, om 1 tijdstap te laten passeren


--onderstaande code dient om factorial.js uit te kunnen voeren
-- main :: IO ()
-- main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
--           contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
--           stdGen <- getStdGen
--           let r = getRandomNumberInRange stdGen 0 42
--           io $ evalStatements (parse parseStatement $ filter (not . isSpace) contents) $ Eval Map.empty Map.empty (return()) $ BlockGame [] (0,0) [] [] r