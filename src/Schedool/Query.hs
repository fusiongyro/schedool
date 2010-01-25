module Schedool.Query (runQuery) where

import Schedool.Section

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
    
data ClassName = Class String Integer 
                 deriving (Show, Eq, Read)

runQuery  :: [Section] -> String -> [Section]
runQuery = undefined

-- | Parses a string of the form "CS 121" and returns the
--   corresponding data structure: Class "CS" 121.
parseClass :: CharParser st Expr
parseClass = do
  dept <- many1 letter
  spaces
  num <- many1 digit
  return $ Value (Class dept (read num))

data Expr = Expr `OrExp` Expr
          | Expr `AndExp` Expr
          | Expr `AndOrExp` Expr
          | Value ClassName
            deriving (Show, Eq, Read)

table = [[Infix (string "," >> return AndExp) AssocLeft,
          Infix (string "and" >> return AndExp) AssocLeft],
         [Infix (string "or" >> return OrExp) AssocLeft],
         [Infix (string "and/or" >> return AndOrExp) AssocLeft]]

exprParser = buildExpressionParser table parseClass
