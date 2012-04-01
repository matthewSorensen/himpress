{-# LANGUAGE OverloadedStrings #-}
module Himpress.Format 
    (Format (..), File (..), formatPresentation)
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

{--
data FormatOpts = FormatOpts {
      meta::[(Text,Text)],
      title::Text,
      headScripts::[Script],
      scripts::[Script],
      style::[Text]
    }
--}
-- Switch everything over to this.
data Format = Format {
      meta::[(Text,Text)]
    , title::Text
    , scripts::[File]
    , style::[File]
    , bare::Bool
    }           
data File = Inline Text | Link Text 


formatPresentation::Format->[Slide]->Html
formatPresentation o = (if bare o then id else full o) . slideDiv . mapM_ slide


full::Format->Html->Html
full opt content = 
    H5.docTypeHtml $ do
      H5.head $ do
        H5.meta ! A5.charset "utf-8"
        H5.title $ toHtml $ title opt
        mapM_ metaTag $ meta opt
        mapM_ stylesheet $ style opt
      H5.body $ do
        fallback ! A5.class_ "fallback-message"
        content
        mapM_ script $ scripts opt
    where metaTag (n,v) = H5.meta ! A5.name (toValue n) ! A5.content (toValue v)
          
          stylesheet (Link url) = H5.link ! A5.rel "stylesheet" ! A5.type_ "text/css" ! A5.href (toValue url)
          stylesheet (Inline t) = H5.style (preEscapedText t) ! A5.type_ "text/css"

          script (Inline t) = H5.script (preEscapedText t) ! A5.type_ "text/javascript"
          script (Link url) = H5.script "" ! A5.type_ "text/javascript" ! A5.src (toValue url)

          fallback = H5.div $ do
             H5.p "Your browser doesn't support the features required for impress.js"
             H5.p "(For best results, use the latest WebKit-based browsers)"

    
          {--
formatPresentation::FormatOpts->[Slide]->Html
formatPresentation opt slides = 
    H5.docTypeHtml $ do
      H5.head $ do
        H5.meta ! A5.charset "utf-8"
        H5.title $ toHtml $ title opt
        mapM_ mkMeta $ meta opt
        mapM_ (mkStyle  . toValue) $ style opt
        mapM_ script $ headScripts opt
      H5.body $ do
        fallback ! A5.class_ "fallback-message"
        slideDiv $ mapM_ slide slides
--        addImpress $ H5.div $ mapM_ formatSlide slides
        mapM_ script $ scripts opt
        H5.script "impress().init();"  ! A5.type_ "text/javascript"
    where mkStyle s    = H5.link ! A5.rel "stylesheet" ! A5.type_ "text/css" ! A5.href s
          mkMeta (n,v) = H5.meta ! A5.name (toValue n) ! A5.content (toValue v)
          addImpress x = x ! A5.id "impress" ! A5.class_ "impress-not-supported"
--}

slideDiv::Html->Html
slideDiv = (! A5.id "impress"). (! A5.class_ "impress-not-supported") . H5.div

slide::Slide->Html
slide (nat,body) = applyAttrs nat $ applyClass nat $ H5.div body
    where applyAttrs nat = flip (foldrWithKey addAttr) (attrs nat)
          addAttr name val = (! (customAttribute (textTag name) $ toValue val))
          applyClass nat = (! toClass nat) 
          toClass = A5.class_ . toValue . intercalate " " . toList . classes
