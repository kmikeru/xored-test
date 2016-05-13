{- LANGUAGE OverloadedStrings -}
module Eval where
import Text.Regex
import Text.Regex.Posix
import Cells
import Data.Maybe
import Debug.Trace

data Item = IntOperand Integer | RefOperand String | Operation String deriving (Show)

evalAll::[[Cell]]->[[Result]]
evalAll cells = map (\r -> map (evalCell cells) r) cells

evalCell::[[Cell]]->Cell->Result
evalCell cells cell=case cell of
  IntegerCell i ->  IntResult i
  StringCell s -> StringResult s
  ExprCell e -> IntResult (eval1 cells (tokenize e))

eval1::[[Cell]]->[Item]->Integer
eval1 cells input=case input of
  [] -> 0
  [IntOperand i]-> trace ("int:"++show i)
    i
  [RefOperand s]-> trace ("RefOperand "++s)
    eval1 cells (resolve cells (RefOperand s))
  (IntOperand i:Operation j:IntOperand k:[])-> trace ("all int end:"++(show i)++j++(show k))
    calc
    where calc = op j i k
  (IntOperand i:Operation j:IntOperand k:rest)-> trace ("all int:"++(show i)++j++(show k)++" rest:"++(show rest))
    eval1 cells  ([IntOperand calc]++rest)
    where calc = op j i k
  (op1:Operation j:op2:rest)-> trace ("last:"++(show op1)++j++(show op2)++" rest:"++(show rest))
    eval1 cells (res1 ++ [Operation j] ++ res2 ++ rest)
    where res1=resolve cells op1
          res2=resolve cells op2

resolve::[[Cell]]->Item->[Item]
resolve cells input = case input of
  IntOperand i -> [IntOperand i]
  RefOperand s -> case getCellByName cells s of
    IntegerCell i -> [IntOperand i]
    ExprCell e -> tokenize e

op "+" = (+)
op "-" = (-)
op "*" = (*)
op "/" = quot

splitStr = matchRegexAll  (mkRegex "\\+|\\*|\\-|\\/")

flatten::[[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

countExprCells::[[Cell]]->Int
countExprCells cells = length $ filter isExprCell $ flatten cells

makeItem::String->Item
makeItem "+" = Operation "+"
makeItem "-" = Operation "-"
makeItem "*" = Operation "*"
makeItem "/" = Operation "/"

makeItem input = case isNum input of
  True -> IntOperand (read input)
  False -> RefOperand input

reduce1::Item->Item->Item->Integer
reduce1 a opS b = op opS1 a1 b1
  where (IntOperand a1)=a
        (IntOperand b1)=b
        (Operation opS1)=opS

tokenize::String->[Item]
tokenize input = map makeItem (getAllTextMatches $ input =~ "([A-Z][0-9]+|[0-9]+|[+-\\*/])"::[String])

isExprCell (ExprCell _) = True
isExprCell _         = False

isRefCell::String->Bool
isRefCell name = case  matchRegex (mkRegex "([A-Z][0-9]+)") name of
  Just _ -> True
  Nothing -> False

isNum::String -> Bool
isNum inStr = case  matchRegex (mkRegex "^([0-9]+)$") inStr of
  Nothing -> False
  Just _ -> True
