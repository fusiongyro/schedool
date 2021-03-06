{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Schedool.Query ( runQuery
					,getQueryContext
                      ,executeQuery
                      ,Catalog
                      ,showResults,
                      module Schedool.Section) where

import Schedool.Data
import Schedool.Overlap
import Schedool.Section

import Control.Applicative ((<$>))
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

-- This should actually be String String, because we have classes like CS-111L
data ClassName = Class String String
                 deriving (Show, Eq, Ord, Read)

-- | The expression type for our language.
data Expr = Expr `OrExp` Expr
          | Expr `AndExp` Expr
          | Expr `AndOrExp` Expr
          | Value ClassName
            deriving (Show, Eq, Read)

-- | This is an internal datatype used for finding the sections rapidly based
--   on the class name.
type Catalog = Map.Map ClassName [Section]

runQuery  :: String -> IO [[Section]]
runQuery q = do
  cat <- getQueryContext
  return $ executeQuery cat q

getQueryContext :: IO Catalog
getQueryContext = buildCatalog <$> getSections

executeQuery :: Catalog -> String -> [[Section]]
executeQuery cat q = case runParse q of
                       Just expr -> winnow $ evaluate cat expr
                       Nothing   -> []

lexer :: forall st. P.TokenParser st
lexer = P.makeTokenParser
        (emptyDef { identStart = letter
                  , identLetter = letter
                  , reservedOpNames = ["and", ",", "or", "and/or"]})

--space      = P.whiteSpace lexer
ident :: forall st. CharParser st String
ident      = P.identifier lexer

classnum :: forall st. GenParser Char st String
classnum   = many1 alphaNum

reservedOp :: forall st. String -> CharParser st ()
reservedOp = P.reservedOp lexer

parens :: forall st a. CharParser st a -> CharParser st a
parens     = P.parens lexer

-- | Parses a string of the form "CS 121" and returns the
--   corresponding data structure: Class "CS" 121.
parseClass :: CharParser st Expr
parseClass = do
  dept <- ident
  num  <- classnum
  spaces
  return $ Value $ Class (upcase dept) (upcase num)
      where
        upcase = map toUpper

term :: forall st. GenParser Char st Expr
term = parseClass <|> (parens exprParser <?> "class")

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
--combinations :: Expr -> [[ClassName]]
--combinations (Value c)          = [[c]]
                                  -- is this really right?
--combinations (e1 `AndExp` e2)   = [ i ++ j | i <- combinations e1, j <- combinations e2 ]
--combinations (e1 `OrExp` e2)    = combinations e1 ++ combinations e2
--combinations (e1 `AndOrExp` e2) = combinations (e1 `AndExp` e2) ++ combinations (e1 `OrExp` e2)

-- | A simple interface to the parser.
runParse :: String -> Maybe Expr
runParse s = case parse exprParser "runParse" s of
               Left _  -> Nothing
               Right e -> Just e

-- | This generates the catalog we need to perform the lookup.
buildCatalog :: [Section] -> Catalog
buildCatalog sects = Map.fromListWith (++) [ (classOf sec, [sec]) | sec <- sects ]
    where
      classOf (Section { sectionOf = c, .. }) = Class (department c) (course c)

evaluate :: Catalog -> Expr -> [[Section]]
evaluate cat (Value c)          = [ [x] | x <- Map.findWithDefault [] c cat]
evaluate cat (e1 `AndExp` e2)   = [ i ++ j | i <- evaluate cat e1, j <- evaluate cat e2]
evaluate cat (e1 `OrExp` e2)    = evaluate cat e1 ++ evaluate cat e2
evaluate cat (e1 `AndOrExp` e2) = evaluate cat (e1 `AndExp` e2) ++ evaluate cat (e1 `OrExp` e2)

winnow :: [[Section]] -> [[Section]]
winnow = filter noOverlaps

showSect :: Section -> String
showSect s = department (sectionOf s) ++ " " ++ course (sectionOf s) ++ "-" ++ show (section s)

showResults :: [[Section]] -> [[String]]
showResults = map (map showSect)

{-

cat = buildCatalog <$> getSections

(Just c1) = runParse "cse 113"
(Just c2) = runParse "phys 122"

-- showResults $ filter Schedool.Overlap.noOverlaps $ query "cse 113 and phys122 and span 589 or span 385"

-}