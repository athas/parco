-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parco
-- Copyright   :  (c) Troels Henriksen 2012-2013
-- License     :  BSD-style (see the LICENSE file)
--
-- Maintainer  :  athas@sigkill.dk
-- Stability   :  stable
-- Portability :  portable
--
-- This module implements a general parser class that you should
-- probably implement for any parsers using Parco - it encapsulates
-- features that cannot be expressed using other standard classes.
-- While some parser combinators (such as "Text.Parco.Perm") can be
-- expressed solely through 'Applicative' and 'Alternative', others
-- may need to do backtracking, which not all parser implementations
-- can do automatically.
--
-- Before defining your own instance of the class, check if a package
-- is available that does it for you.  For example, @parco-parsec@
-- defines an instance for Parsec.
--
-----------------------------------------------------------------------------

module Text.Parco
  ( Parser(..)
  ) where

import Control.Applicative

-- | Some parser combinators need special operations that are not
-- captured in any of the standard typeclasses (backtracking, for
-- example).  For these, it is necessary to define an instance of this
-- class.
class Alternative p => Parser p where
  -- | If @try p@ fails, no input should be consumed.  The default
  -- method calls 'error'.
  try :: p a -> p a
  try = error "Cannot backtrack"
  -- | If @expects p s@ fails, the error message should note that @s@
  -- (a descriptive string) was expected, but has otherwise no
  -- semantic effect on the parser.  The default method simply returns
  -- @p@.
  expects :: p a -> String -> p a
  expects p _ = p
