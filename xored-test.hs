{- LANGUAGE OverloadedStrings -}
import System.IO (isEOF)
import Data.Char
import Text.Regex

main = mainLoop

mainLoop = do
  done <- isEOF
  if done then putStrLn "bye"
  else do
    input<-getLine
    let result = evalStr input
    putStrLn $ show result
    mainLoop

evalStr::String->Maybe Integer
evalStr inpStr = case splitStr inpStr of
  Nothing -> Nothing
  Just (beforeMatch, matched, afterMatch,_) -> Just (reduce (read beforeMatch :: Integer) matched (read afterMatch::Integer))

reduce::Integer->String->Integer->Integer
reduce a opS b = op opS a b

op "+" = (+)
op "-" = (-)
op "*" = (*)
op "/" = quot

splitStr = matchRegexAll  (mkRegex "\\+|\\*|\\-|\\/")
