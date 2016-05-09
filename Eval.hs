{- LANGUAGE OverloadedStrings -}
module Eval where
import Text.Regex
import Text.Regex.Posix
import Cells
import Data.Maybe
import Debug.Trace

data Item = IntOperand Integer | RefOperand String | Operation String deriving (Show)

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

res::[[Cell]]->Item->[Item]
res cells input = case input of
  IntOperand i -> [IntOperand i]
  RefOperand s -> case getCellByName cells s of
    IntegerCell i -> [IntOperand i]
    ExprCell e -> tokenize e

eval1::[[Cell]]->[Item]->Integer
eval1 cells input=case input of
  [] -> -1
  [IntOperand i]->i
  [RefOperand s]->eval1 cells $ take 1 (res cells (RefOperand s))
  (IntOperand i:Operation j:IntOperand k:[])-> calc
    where calc = op j i k
  (IntOperand i:Operation j:IntOperand k:rest)-> eval1 cells  ([IntOperand calc]++rest)
    where calc = op j i k
  (op1:Operation j:op2:rest)-> traceShow res1
    eval1 cells (res1 ++ [Operation j] ++ res2 ++ rest)
    where res1=res cells op1
          res2=res cells op2


{-
evalExprCell::[[Cell]]->Cell->Cell
evalExprCell cells cell = case cell of
  ExprCell e -> IntegerCell (fromJust $ evalStr cells e)
  otherwise -> cell

pattern =  matchRegexAll (mkRegex "([0-9]+|[A-Z][0-9]+)(\\+|\\*|\\-|\\/)([0-9]+|[A-Z][0-9]+)")
evalStr::[[Cell]]->String->Maybe Integer
evalStr cells inpStr = case pattern inpStr of
  Nothing -> case isNum inpStr of -- нет ни одной операции, значит это либо число (=33, например) или ссылка на ячейку (=B2)
    True  -> Just (read inpStr::Integer)
    False -> Nothing
  Just (beforeMatch, matched, afterMatch,matchedList) -> case afterMatch of
    "" ->  immediate -- больше ничего не осталось, вычисляем и возвращаем результат
    otherwise ->
      -- trace ("rec:"++(show immediate)++afterMatch)
      evalStr cells (show immediate ++ afterMatch) -- собираем новую строку, подставляя вычисленное значение и вычисляем рекурсивно
    where immediate = Just (reduce leftOp op rightOp )
          leftOp=(read replacedLeft::Integer)
          rightOp=(read replacedRight::Integer)
          replacedLeft=replaceRef cells (matchedList !! 0)
          replacedRight=replaceRef cells (matchedList!! 2)
          op=(matchedList !! 1)

replaceRef::[[Cell]]->String->String
replaceRef cells input = case isRefCell input of
  False -> read input -- уже число
  otherwise -> case ref of
    IntegerCell i ->  show i -- ссылка на числовую ячейку
    ExprCell e -> show $evalExprCell cells (ExprCell e)
  where ref=getCellByName cells input


reduce::Integer->String->Integer->Integer
reduce a opS b = op opS a b
-}
op "+" = (+)
op "-" = (-)
op "*" = (*)
op "/" = quot

splitStr = matchRegexAll  (mkRegex "\\+|\\*|\\-|\\/")



{- evalCell2::Cell->Cell
evalCell2 c = case c of
  IntegerCell i ->  IntegerCell i
  StringCell s -> StringCell s
  ExprCell e -> case evalStr e of
              Nothing -> ExprCell e
              Just res -> IntegerCell res

evalExprCell::Cell->Cell
evalExprCell cell = undefined
  --case (length $ findDependency cell) of
  --0 -> IntegerCell 0
  --otherwise -> IntegerCell 1

evalAll::[[Cell]]->[[Cell]]
evalAll cells = map (\r -> map evalCell2 r) cells

flatten::[[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

countExprCells::[[Cell]]->Int
countExprCells cells = length $ filter isExprCell $ flatten cells

-}
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
