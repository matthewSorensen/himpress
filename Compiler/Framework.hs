{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Compiler.Framework where

import Compiler.Transitions
import Data.Attoparsec.Text
import Data.Text (Text)
import Data.Map (Map)
import Control.Applicative

type Element  = Either Transition Text
type Format a = Either (a->Element) (a->Text->Element)

data DocMode = forall a . Mode {
      name::Text,
      parser::Parser a,
      format::Format a
    }

literal::DocMode 
literal = Mode {name = "literal", parser = pure (), format = Right fmt}
    where fmt = const Right
          
runParser::Parser a->Text->a
runParser p t = either (flail t) id $ parseOnly p t
    where flail t err = error $ concat ["Parser failed with error '",err,"' on text ",show t]

apply::DocMode->Text->Text->(Element,Maybe Text)
apply (Mode _ parse form) opts text = either l r form
    where parsed = runParser parse opts
          l    f = (f parsed, Just text)
          r    f = (f parsed text, Nothing)
