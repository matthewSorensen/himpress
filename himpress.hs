{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Himpress.Framework
import Himpress.Compile
import qualified Himpress.Format as F
import Himpress.Modes

import System (getArgs)
import Prelude hiding (writeFile,readFile,words,putStrLn,getContents)
import Data.Text.IO (readFile,getContents) 
import Data.Text.Lazy.IO (writeFile,putStrLn)
import Text.Blaze.Renderer.Text (renderHtml)
import Data.Text (pack,words)
import System.Console.CmdArgs

data Opts = Opts {
      size::(Int,Int)
    , input::String
    , output::String
    , css::String
    , js::String
    , impress::String
    , title::String
    } deriving (Show,Data,Typeable)

opts = Opts {
         size    = (1000,1000) &= help "Default size (in pixels) for each step"
       , input   = "" &= typFile
       , output  = "" &= typFile
       , css     = "" &= typ listType &= help "CSS files to include in the presentation"
       , js      = "" &= typ listType &= help "JavaScript files to include in the presentation"
       , impress = "js/impress.js" &= typFile &= help "Path to impress.js script" 
       , title   = "" &= typ "STRING" &= help "Title for the presentation"
       } &= summary "himpress v0.0, (c) Matthew Sorensen 2012"

listType="< space-separated list of paths >"

cmdOptsToFormat c = F.FormatOpts {
                      F.meta = []
                    , F.title = pack $ if title c /= "" then title c else output c
                    , F.headScripts = splitIntoList $ js c
                    , F.scripts = (:[]) $ pack $ impress c
                    , F.style = splitIntoList $ css c
                    } where splitIntoList = words . pack

generatePresentation o = renderHtml . format . comp . parse
    where format = F.formatPresentation (cmdOptsToFormat o) 
          comp   = compile $ size o
          parse  = parsePresentation defaultMode allModes

inputAction o
    | input o == "" = getContents
    | otherwise     = readFile $ input o
outputAction o
    | output o == "" = putStrLn
    | otherwise      = writeFile $ output o

main = do
  o <- cmdArgs opts
  inputAction o >>= outputAction o . generatePresentation o
