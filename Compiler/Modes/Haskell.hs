{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Compiler.Modes.Haskell 
    (haskellModes)
    where



import Compiler.Framework

import Prelude hiding (takeWhile)
import Data.Text (Text,pack,unpack)
import Data.Attoparsec.Text
import Control.Applicative
import Data.Char (isSeparator)

import Text.Blaze.Renderer.Text (renderHtml)
import Text.Blaze 
import qualified Text.Blaze.Html5 as H5
import qualified Text.Blaze.Html5.Attributes as A5
import Data.Text.Lazy (toStrict)
import Data.Monoid (mempty)
import Language.Haskell.HsColour


haskellModes = [haskBox,colour]

haskBox = Mode {name = "haskell-box",parser = parseCredit,format = Right $ (Left .). form}
    

parseCredit::Parser (Maybe (Text,Text))
parseCredit = skipSpace *> ((Nothing <$ endOfInput) <|> (url >>= title))
    where url = takeTill isSeparator 
          title t = skipSpace *> ((Just (t,t) <$ endOfInput) <|> ((Just . (t,)) <$> takeText))

form::Maybe (Text,Text)->Text->Html
form cred src = (! A5.class_ "code") $ H5.div $ colourize src >> maybe mempty makeLink cred
    where makeLink (href,text) = H5.hr >> (H5.a $ toHtml text) ! A5.href (toValue href)

colour = Mode {name = "hscolour", 
               parser = return (), 
               format = Right $ const $ Left . colourize}

-- There is soooo much wrong with this function.
-- First, no native default ColourPrefs - I think the undefined is safe.
-- Then the boolean blindness that the second two falses suffer from.
-- Then that it isn't natively Text.
colourize = makePre . hscolour CSS undefined False True "" False . unpack 
    where makePre = (! A5.class_ "haskell") . H5.pre . preEscapedString

