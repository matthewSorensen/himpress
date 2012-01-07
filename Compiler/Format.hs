{-# LANGUAGE OverloadedStrings #-}
module Compiler.Format where

import Compiler.Transitions (Slide,Native (..))

import Data.Set (fold)
import Data.Map (foldrWithKey)
import Data.Text (Text)
import Data.Monoid
import Prelude hiding (div)
import Text.Blaze.Html5.Attributes (class_)
import Text.Blaze.Html5 (div)
import Text.Blaze (toHtml,toValue,textTag,customAttribute,Html,(!))
import Text.Blaze.Renderer.Utf8 (renderHtml)

(<>)::Monoid a=>a->a->a
(<>) = mappend

formatSlide::Slide->Html
formatSlide (nat,body) = applyAttrs nat $ applyClass nat $ div $ toHtml body
    where applyAttrs nat = flip (foldrWithKey addAttr) (attrs nat)
          addAttr name val = (! (customAttribute (textTag name) $ toValue val))
          applyClass nat = (! toClass nat) 
          toClass = class_ . toValue . fold (((<> " ") .). (<>)) mempty . classes
