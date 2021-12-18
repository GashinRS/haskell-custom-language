import Parser
import Data.Char
import Interpreter

import System.Environment (getArgs)

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          snd $ evalStatements (parse parseStatement $ filter (not . isSpace) contents) []
          --snd $ evalStatements (parse parseStatement "if((1==1)){print(2);}") []
          --evalPrint $ parse parsePrint "print(3);"
          --putStrLn contents