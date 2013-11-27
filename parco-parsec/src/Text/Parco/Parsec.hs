{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 'Text.Parco.Parser' instance for 'Text.Parsec.ParsecT'.
module Text.Parco.Parsec() where

import qualified Text.Parco
import qualified Text.Parsec

instance Text.Parco.Parser (Text.Parsec.ParsecT s u m) where
  try = Text.Parsec.try
  expects = (Text.Parsec.<?>)
