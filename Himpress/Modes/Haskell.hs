{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Himpress.Modes.Haskell 
    (haskellModes)
    where

import Himpress.Framework

import Data.Text (unpack)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char (isSeparator)

import Text.Blaze.Renderer.Text (renderHtml)
import Text.Blaze 
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as A5
import Language.Haskell.HsColour

haskellModes = [haskBox,colour]

haskBox = Mode {name = "haskell-box",parser = parseCredit,format = Right $ (Left .). form}
    
parseCredit = skipSpace *> ((Nothing <$ endOfInput) <|> (url >>= title))
    where url = takeTill isSeparator 
          title t = skipSpace *> ((Just (t,t) <$ endOfInput) <|> ((Just . (t,)) <$> takeText))

form cred src = (! A5.class_ "code") $ H5.div $ colourize src >> maybe (return ()) makeLink cred
    where makeLink (href,text) = H5.hr >> H5.a (toHtml text) ! A5.href (toValue href) ! A5.target "_blank"

colour = Mode {name = "hscolour", 
               parser = return (), 
               format = Right $ const $ Left . colourize}

-- There is soooo much wrong with this function.
-- First, no native default ColourPrefs - I think the undefined is safe.
-- Then the boolean blindness that the second two falses suffer from.
-- Then that it isn't natively Text.
colourize = makePre . hscolour CSS undefined False True "" False . unpack 
    where makePre = (! A5.class_ "haskell") . H5.pre . preEscapedString
