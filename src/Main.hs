import Parser
import Data.Char
import Interpreter

import System.Environment (getArgs)

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          --print $ parse parseStatement (filter (\xs -> (xs /=' ')) contents)
          evalStatements $ parse parseStatement $ filter (not . isSpace) contents
          --evalPrint $ parse parsePrint "print(3);"
          --putStrLn contents