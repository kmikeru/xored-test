{- LANGUAGE OverloadedStrings -}
module Cells where
import Text.Regex
import Text.Regex.Posix

data Cell = IntegerCell Integer | StringCell String | ExprCell String deriving (Show)

strToCells::String->[Cell]
strToCells input = map strToCell (splitRegex (mkRegex "\t") input)

strToCell::String->Cell
strToCell input = case (head input) of
  '\'' -> StringCell (tail input)
  '=' -> ExprCell (tail input)
  otherwise -> IntegerCell (read input::Integer)

findDependency::String->[String]
findDependency input= getAllTextMatches $ input =~ "([A-Z][0-9]+)"::[String]
