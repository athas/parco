-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parco.Expr
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007, (c) Troels Henriksen 2012-2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  athas@sigkill.dk
-- Stability   :  stable
-- Portability :  portable
--
-- This module implements permutation parsers, and is a generalisation
-- of 'Text.Parsec.Expr' that will work with any parser combinator
-- library. It builds a parser given a table of operators and
-- associativities.
--
-- This module is a drop-in replacement for 'Text.Parsec.Expr', and
-- the implementation is taken from that module.
--
-----------------------------------------------------------------------------

module Text.Parco.Expr
  ( Assoc(..), Operator(..), OperatorTable
  , buildExpressionParser
  ) where

import Control.Applicative
import Text.Parco

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------

-- | This data type specifies the associativity of operators: left, right
-- or none.
data Assoc = AssocNone
           | AssocLeft
           | AssocRight

-- | This data type specifies operators that work on values of type
-- @a@.  An operator is either binary infix or unary prefix or
-- postfix. A binary operator has also an associated associativity.
-- As in Parsec, 'Infix' and 'Prefix' operators cannot be directly
-- nested - i.e, in an expression grammar, if @-@ is a prefix
-- operator, @- -x@ would be a parse error, although @-(-x)@ would
-- work.  Use 'PrefixNestable'/'PostfixNestable' if you want fully
-- nestable unary operators.
data Operator p a = Infix (p (a -> a -> a)) Assoc
                  | Prefix (p (a -> a))
                  | Postfix (p (a -> a))
                  | PrefixNestable (p (a -> a))
                  | PostfixNestable (p (a -> a))

-- | An @OperatorTable p a@ is a list of @Operator p a@
-- lists. The list is ordered in descending
-- precedence. All operators in one list have the same precedence (but
-- may have a different associativity).
type OperatorTable p a = [[Operator p a]]

-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------

-- | @buildExpressionParser table term@ builds an expression parser for
-- terms @term@ with operators from @table@, taking the associativity
-- and precedence specified in @table@ into account. Prefix and postfix
-- operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). Prefix and postfix operators
-- of the same precedence associate to the left (i.e. if @++@ is
-- postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- The @buildExpressionParser@ takes care of all the complexity
-- involved in building expression parser. Here is an example of an
-- expression parser that handles prefix signs, postfix increment and
-- basic arithmetic.
--
-- >  expr    = buildExpressionParser table term
-- >          <?> "expression"
-- >
-- >  term    =  parens expr 
-- >          <|> natural
-- >          <?> "simple expression"
-- >
-- >  table   = [ [prefix "-" negate, prefix "+" id ]
-- >            , [postfix "++" (+1)]
-- >            , [binary "*" (*) AssocLeft, binary "/" (div) AssocLeft ]
-- >            , [binary "+" (+) AssocLeft, binary "-" (-)   AssocLeft ]
-- >            ]
-- >
-- >  binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
-- >  prefix  name fun       = Prefix (do{ reservedOp name; return fun })
-- >  postfix name fun       = Postfix (do{ reservedOp name; return fun })
buildExpressionParser :: (Monad p, Parser p) => OperatorTable p a
                      -> p a
                      -> p a
buildExpressionParser operators simpleExpr =
  foldl makeParser simpleExpr operators
  where
    makeParser term ops =
      let (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
            foldr splitOp ([],[],[],[],[],[],[]) ops
          rassocOp   = choice rassoc
          lassocOp   = choice lassoc
          nassocOp   = choice nassoc
          nprefixOp  = choice nprefix `expects` ""
          npostfixOp = choice npostfix `expects` ""
          prefixOp   = choice prefix `expects` ""
          postfixOp  = choice postfix `expects` ""

          ambiguous assoc op = try $ op >> fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")

          ambiguousRight = ambiguous "right" rassocOp
          ambiguousLeft  = ambiguous "left" lassocOp
          ambiguousNon   = ambiguous "non" nassocOp

          termP      = do pre  <- prefixP
                          x    <- term
                          post <- postfixP
                          return (post (pre x))

          postfixP   = do p <- postfixOp
                          more <- postfixP
                          return (p . more)
                       <|> npostfixOp <|> return id

          prefixP    = do p <- prefixOp
                          more <- prefixP
                          return (p . more)
                       <|> nprefixOp <|> return id

          rassocP x  = do f <- rassocOp
                          y <- rassocP1 =<< termP
                          return (f x y)
                       <|> ambiguousLeft
                       <|> ambiguousNon

          rassocP1 x = rassocP x <|> return x

          lassocP x  = do f <- lassocOp
                          y <- termP <|> buildExpressionParser operators simpleExpr
                          lassocP1 (f x y)
                       <|> ambiguousRight
                       <|> ambiguousNon

          lassocP1 x = lassocP x <|> return x

          nassocP x  = do f <- nassocOp
                          y <- termP
                          ambiguousRight
                            <|> ambiguousLeft
                            <|> ambiguousNon
                            <|> return (f x y)

         in do x <- termP
               (rassocP x <|> lassocP x <|> nassocP x <|> return x)
                         `expects` "operator"

    splitOp (Infix op assoc) (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
      case assoc of AssocNone  -> (rassoc,lassoc,op:nassoc,nprefix,npostfix,prefix,postfix)
                    AssocLeft  -> (rassoc,op:lassoc,nassoc,nprefix,npostfix,prefix,postfix)
                    AssocRight -> (op:rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix)

    splitOp (Prefix op) (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
      (rassoc,lassoc,nassoc,op:nprefix,npostfix,prefix,postfix)

    splitOp (Postfix op) (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
      (rassoc,lassoc,nassoc,nprefix,op:npostfix,prefix,postfix)

    splitOp (PrefixNestable op) (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
      (rassoc,lassoc,nassoc,nprefix,npostfix,op:prefix,postfix)

    splitOp (PostfixNestable op) (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,postfix) =
      (rassoc,lassoc,nassoc,nprefix,npostfix,prefix,op:postfix)

-- | Pick the first one that works.
choice :: Alternative a => [a b] -> a b
choice = foldl (<|>) empty
