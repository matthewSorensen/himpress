{-# LANGUAGE OverloadedStrings #-}
module Compiler.Modes.Text 
    (markdown,textModes)
    where

import Compiler.Framework

-- :( :( :( Pandoc should be able to deal with Text
import Data.Text (pack,unpack)
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared
import Text.Pandoc.Parsing
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Readers.Textile
import Text.Pandoc.Readers.RST
import Text.Pandoc.Readers.LaTeX

import Text.Blaze.Renderer.Text (renderHtml)
import Text.Blaze ((!),toHtml)
import Text.Blaze.Html5 (pre)
import Text.Blaze.Html5.Attributes (class_)
import Data.Text.Lazy (toStrict)

markdown = pandoc "markdown" readMarkdown
textModes = [html
            ,markdown
            ,literal
            ,pandoc "textile" readTextile
            ,pandoc "rst"     readRST                     
            ,pandoc "latex"   readLaTeX]
-- Just echo a section of html out.
html = Mode {name = "html", parser = return (), format = Right $ const $ Left}
-- Escape a section of text and enclose it in a pre-tag
literal = Mode {name = "literal", parser = return (), format = Right $ const makePre}
    where makePre x = Left $ toStrict $ renderHtml $ (pre $ toHtml x) ! class_ "literal"

pandoc n reader = Mode {name = n, parser = return (), format = Right $ pandocify reader}
    where pandocify r _ = Left . pack . writeHtmlString writerOpts .  r parserState .  unpack
          parserState = defaultParserState
          writerOpts  = defaultWriterOptions {writerHtml5 = False, writerStandalone = False}
