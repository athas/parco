{-# LANGUAGE NoMonomorphismRestriction #-}
module Text.Parco.Test where

import Control.Applicative

import Data.Char

import qualified Text.Parco
import Text.Parco.Expr
import Text.Parsec hiding ((<|>))
import Text.Parsec.String

data Expr = Num Int
          | Negate Expr
          | Plus Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Increment Expr
            deriving (Show)

instance Text.Parco.Parser (ParsecT s u m) where
  try = Text.Parsec.try

expr    = buildExpressionParser table term
        <?> "expression"

term    =  (char '(' *> expr <* char ')')
        <|> Num . read <$> many1 (satisfy isDigit)
        <?> "simple expression"

table   = [ [prefixAssoc "-" Negate, prefixAssoc "+" id ]
          , [postfix "++" Increment]
          , [binary "*" Mult AssocLeft, binary "/" Div AssocLeft ]
          , [binary "+" Plus AssocLeft, binary "-" Sub   AssocLeft ]
          ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator Parser a
binary  name fun assoc = Infix (string name >> return fun) assoc
prefixAssoc :: String -> (a -> a) -> Operator Parser a
prefixAssoc  name fun       = PrefixNestable (string name >> return fun)
postfix :: String -> (a -> a) -> Operator Parser a
postfix name fun       = Postfix (string name >> return fun)
