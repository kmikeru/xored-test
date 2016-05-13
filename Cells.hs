{- LANGUAGE OverloadedStrings -}
module Cells where
import Text.Regex
import Text.Regex.Posix
import Data.List
import Data.Maybe

data Cell = IntegerCell Integer | StringCell String | ExprCell String deriving (Show)
data Result = IntResult Integer | ErrorResult String | StringResult String deriving (Show)

showResults::[[Result]]->IO()
showResults results=
  mapM_ (\r-> putStrLn $ showResultRow r) results

showResultRow::[Result]->String
showResultRow row =
  intercalate "\t" s ::String
  where s=map (\e->  showResult e) row

showResult::Result->String
showResult r=case r of
  IntResult i->show i
  StringResult s->s

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

mapInd f l = zipWith f l [0..]

{- buildDep::[[Cell]]->[[String]]
buildDep cells = mapInd (\r i -> buildDepRow r) cells

buildDepRow::[Cell]->[String]
buildDepRow cells = mapInd (\e i-> show i ++ show e) cells -}
