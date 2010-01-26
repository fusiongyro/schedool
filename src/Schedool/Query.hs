{-# LANGUAGE RecordWildCards #-}

module Schedool.Query (runQuery) where

import Schedool.Data
import Schedool.Section

import qualified Data.Map as Map

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- This should actually be String String, because we have classes like CS-111L
data ClassName = Class String Integer
                 deriving (Show, Eq, Ord, Read)

runQuery  :: [Section] -> String -> [[Section]]
runQuery = undefined

lexer = P.makeTokenParser
        (emptyDef { identStart = letter
                  , identLetter = letter
                  , reservedOpNames = ["and", ",", "or", "and/or"]})

space = P.whiteSpace lexer
ident = P.identifier lexer
integer = P.integer lexer
reservedOp = P.reservedOp lexer
parens = P.parens lexer

-- | Parses a string of the form "CS 121" and returns the
--   corresponding data structure: Class "CS" 121.
parseClass :: CharParser st Expr
parseClass = do
  dept <- ident
  num <- integer
  return $ Value (Class dept num)

term = parseClass <|> (parens exprParser <?> "class")

-- | The expression type for our language.
data Expr = Expr `OrExp` Expr
          | Expr `AndExp` Expr
          | Expr `AndOrExp` Expr
          | Value ClassName
            deriving (Show, Eq, Read)

-- | The operator precedence expression table for our language.
table :: OperatorTable Char st Expr
table = [[op "," AndExp, op "and" AndExp],
         [op "or" OrExp],
         [op "and/or" AndOrExp]]
    where
      op o cons = Infix (reservedOp o >> return cons) AssocLeft

exprParser :: CharParser st Expr
exprParser = buildExpressionParser table term <?> "expression"


-- | Produces all of the valid combinations of this expression, as a test of the parser.
combinations :: Expr -> [[ClassName]]
combinations (Value c)          = [[c]]
                                  -- is this really right?
combinations (e1 `AndExp` e2)   = [ i ++ j | i <- combinations e1, j <- combinations e2 ]
combinations (e1 `OrExp` e2)    = combinations e1 ++ combinations e2
combinations (e1 `AndOrExp` e2) = combinations (e1 `AndExp` e2) ++ combinations (e1 `OrExp` e2)

-- | A simple interface to the parser.
runParse :: String -> Maybe Expr
runParse s = case parse exprParser "runParse" s of
               Left _  -> Nothing
               Right e -> Just e

-- | This is an internal datatype used for finding the sections rapidly based
--   on the class name.
type Catalog = Map.Map ClassName [Section]

{-
buildCatalog :: [Section] -> Catalog
buildCatalog sects = Map.fromListWith (++) [ (classOf sec, [sec]) | sec <- sects ]
    where
      classOf (Section { sectionOf = c, .. }) = Class (department c) (course
      c)
-}