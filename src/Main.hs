import Parser
import Data.Char
import Interpreter
import qualified Data.Map as Map
import Engine
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import System.Random (StdGen, getStdGen, randomR)
import System.Environment (getArgs)


import TempMain

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          let all = evalStatements (parse parseStatement $ filter (not . isSpace) contents) $ Eval Map.empty Map.empty (return()) $ BlockGame (0,0) [] (0,0) [] []
          let funs = functions all
          let start = callGameFunction "startGame" all
          stdGen <- getStdGen
          let r = getRandomNumberInRange stdGen 0 $ height*width-1
          play (InWindow "UGent Brick Game" (500, 800) (10, 10))
                    screenGreen -- de achtergrondkleur
                    2 -- aantal stappen per seconde
                    (startGame r start)-- de beginwereld
                    gamePic -- de 'render'-functie, om naar scherm te tekenen
                    move -- de 'handle'-functie, om gebruiksinvoer te verwerken
                    next -- de 'step'-functie, om 1 tijdstap te laten passeren