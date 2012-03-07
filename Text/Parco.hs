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

class Alternative p => Parser p where
  try :: p a -> p a
  try = error "Cannot backtrack"
  expects :: p a -> String -> p a
  expects p _ = p
