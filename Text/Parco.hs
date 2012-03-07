-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parco
-- Copyright   :  (c) Troels Henriksen 2012
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  athas@sigkill.dk
-- Stability   :  stable
-- Portability :  portable
-- 
-- This module implements a general parser class that you should
-- probably implement for any parsers using Parco.
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
