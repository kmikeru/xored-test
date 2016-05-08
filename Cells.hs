{- LANGUAGE OverloadedStrings -}
module Cells where
import Text.Regex
import Text.Regex.Posix
import Data.List
import Data.Maybe

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

getCellByName::[[Cell]]->String->Cell
getCellByName inarray index =
  --let (row,col) = cellIndex index
  --let (row,col)=(0,0)
  inarray !! row !! col
  where (row,col) = cellIndex index

cellIndex::String->(Int,Int)
cellIndex input = do
  let digits=read (tail input)
  let colIndex=letterIndex $ head input
  (digits-1,fromInteger colIndex)

letterIndex::Char->Integer
letterIndex input =
  toInteger $ fromJust $ elemIndex input ['A'..'Z']
