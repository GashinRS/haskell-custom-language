import Parser
import Data.Char
import Interpreter
import qualified Data.Map as Map

import System.Environment (getArgs)

main :: IO ()
main = do (filename:_) <- getArgs -- het eerste argument (stack run tetris.xyz)
          contents <- readFile filename -- bevat de inhoud van tetris.xyz als String
          snd $ evalStatements (parse parseStatement $ filter (not . isSpace) contents) Map.empty
          --evalPrint $ parse parsePrint "print(3);"
          --putStrLn $ filter (not . isSpace) contents