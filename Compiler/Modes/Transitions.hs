{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Compiler.Modes.Transitions 
    (transitionsModes)
    where

import Compiler.Transitions
import Compiler.Framework

import Data.Attoparsec.Text hiding (D)
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Map (singleton)
import Data.Set (insert)
import Data.Text (pack,strip)
import Data.Monoid
import Data.Char (isSeparator)
-- class <class lies>
transitionsModes = [new,slideId,classMode,attribute]
-- Emit a non-composable null move to start a new slide.
new = Mode {name = "new-slide", parser = return (), format = Left fmt}
    where fmt = const $ Right $ Right (Move $ Coord (0,0,0),False)
-- >>> id <id>
slideId = Mode {name = "id", parser = skipSpace *> takeTill isSeparator, format = Left makeNative }
    where makeNative i = Right $ Left (mempty {attrs = singleton "id" i}, True)
-- class <class list>
classMode = Mode {name = "class", parser = parseClassSet, format = Left makeNative}
    where makeNative s   = Right $ Left (mempty {classes = s}, True) 
          parseClassSet  = skipSpace *> ((mempty <$ endOfInput) <|> (insert <$> takeTill isSeparator <*> parseClassSet))
-- attribute <attribute> <value>
attribute = Mode {name = "attribute", parser = parseAttr, format = Left makeNative}
    where makeNative i = Right $ Left (mempty { attrs = uncurry singleton i}, True)
          parseAttr    = skipSpace *> ((,) <$> takeTill isSeparator <*> (strip <$> takeText))
-- some way of controlling global opacity would be really nice. Or even next opacity.