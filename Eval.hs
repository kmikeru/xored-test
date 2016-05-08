{- LANGUAGE OverloadedStrings -}
module Eval where
import Text.Regex
import Cells

evalCell::Cell->String
evalCell c = case c of
  IntegerCell i -> show i
  StringCell s -> s
  ExprCell e -> show $ evalStr e

evalStr::String->Maybe Integer
evalStr inpStr = case pattern inpStr of
  Nothing -> Nothing
  Just (beforeMatch, matched, afterMatch,matchedList) -> case afterMatch of
    "" ->  immediate -- больше ничего не осталось, вычисляем и возвращаем результат
    otherwise ->
      -- trace ("rec:"++(show immediate)++afterMatch)
      evalStr $ (show immediate) ++ afterMatch -- собираем новую строку, подставляя вычисленное значение и вычисляем рекурсивно
    where immediate = Just (reduce (read (matchedList !! 0) :: Integer) (matchedList !! 1) (read (matchedList!! 2)::Integer))

reduce::Integer->String->Integer->Integer
reduce a opS b = op opS a b

op "+" = (+)
op "-" = (-)
op "*" = (*)
op "/" = quot

splitStr = matchRegexAll  (mkRegex "\\+|\\*|\\-|\\/")

isNum::String -> Bool
isNum inStr = case  matchRegex (mkRegex "^([0-9]+)$") inStr of
  Nothing -> False
  Just _ -> True

pattern =  matchRegexAll (mkRegex "([0-9]+)(\\+|\\*|\\-|\\/)([0-9]+)")
