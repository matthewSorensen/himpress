{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Compiler.Framework where

import Compiler.Transitions
import Data.Attoparsec.Text
import Data.Text (Text)
import Control.Applicative

type Element = Either Transition Text

data DocMode = forall a . Mode {
      name::Text,
      parser::Parser a,
      format::a->Text->Element,
      continues::Bool
    }

literal::DocMode 
literal = Mode {name = "literal", parser = pure (), format = const Right, continues = False}
          
runParser::Parser a->Text->a
runParser p t = either (flail t) id $ parseOnly p t
    where flail t err = error $ concat ["Parser failed with error '",err,"' on text ",show t]

apply::DocMode->Text->Text->(Element,Maybe Text)
apply m opts text = let app (Mode _ p f _) = f (runParser p opts) -- Pattern match to deal with the existential
                        partial = app m
                    in if continues m then (partial text, Nothing) else (partial "", Just text)
