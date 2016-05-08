{- LANGUAGE OverloadedStrings -}
import System.IO (isEOF)
--import Data.Char
import Text.Regex
import Data.Maybe()
import Debug.Trace()
import Eval
import Cells

main = mainLoop

mainLoop = do
  done <- isEOF
  if done then putStrLn "bye"
  else do
    input<-getLine
    --let result = evalStr input
    let result = strToCells input
    print result
    mainLoop

readToCells::IO [[Cell]]
readToCells = do
  contents <-readFile "input.txt"
  let fileLines = lines contents
  let cells=map strToCells fileLines
  return cells
  --return fileLines
