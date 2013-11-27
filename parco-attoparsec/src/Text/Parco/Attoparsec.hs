{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | 'Text.Parco.Parser' instances for
-- 'Data.Attoparsec.ByteString.Parser' and
-- 'Data.Attoparsec.Text.Parser'
module Text.Parco.Parsec() where

import qualified Text.Parco
import qualified Data.Attoparsec.ByteString
import qualified Data.Attoparsec.Text

instance Text.Parco.Parser Data.Attoparsec.ByteString.Parser where
  try = Data.Attoparsec.ByteString.try
  expects = (Data.Attoparsec.ByteString.<?>)

instance Text.Parco.Parser Data.Attoparsec.Text.Parser where
  try = Data.Attoparsec.Text.try
  expects = (Data.Attoparsec.Text.<?>)
