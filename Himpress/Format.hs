{-# LANGUAGE OverloadedStrings #-}
module Himpress.Format 
    (FormatOpts (..), formatPresentation)
        where

import Himpress.Transitions (Slide,Native (..))

import Data.Set (toList)
import Data.Map (foldrWithKey)
import Data.Text (Text,intercalate)
import Data.Monoid
import Control.Monad (mapM_)

import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as A5
import Text.Blaze 
import Text.Blaze.Renderer.Utf8 (renderHtml)

data FormatOpts = FormatOpts {
      meta::[(Text,Text)],
      title::Text,
      headScripts::[Text],
      scripts::[Text],
      style::[Text]
    }

formatSlide::Slide->Html
formatSlide (nat,body) = applyAttrs nat $ applyClass nat $ H5.div body
    where applyAttrs nat = flip (foldrWithKey addAttr) (attrs nat)
          addAttr name val = (! (customAttribute (textTag name) $ toValue val))
          applyClass nat = (! toClass nat) 
          toClass = A5.class_ . toValue . intercalate " " . toList . classes
          

formatPresentation::FormatOpts->[Slide]->Html
formatPresentation opt slides = 
    H5.docTypeHtml $ do
      H5.head $ do
        H5.meta ! A5.charset "utf-8"
        H5.title $ toHtml $ title opt
        mapM_ mkMeta $ meta opt
        mapM_ (mkStyle  . toValue) $ style opt
        mapM_ (mkScript . toValue) $ headScripts opt
      H5.body $ do
        addImpress $ H5.div $ do
                       fallback ! A5.class_ "fallback-message"
                       mapM_ formatSlide slides
        mapM_ (mkScript . toValue) $ scripts opt
    where mkStyle s    = H5.link ! A5.rel "stylesheet" ! A5.type_ "text/css" ! A5.href s
          mkScript s   = H5.script "" ! A5.type_ "text/javascript" ! A5.src s
          mkMeta (n,v) = H5.meta ! A5.name (toValue n) ! A5.content (toValue v)
          addImpress x = x ! A5.id "impress" ! A5.class_ "impress-not-supported"

fallback = H5.div $ do
             H5.p "Your browser doesn't support the features required for impress.js"
             H5.p "(For best results, use the latest WebKit-based browsers)"
