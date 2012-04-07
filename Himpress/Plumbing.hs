{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Himpress.Plumbing where
import qualified Himpress.Format as F
import Paths_himpress

import System.INotify
import System.FilePath (takeExtension)

import Control.Concurrent (threadDelay)
import Control.Arrow ((&&&),(***))
import Control.Monad (mapM_)

import Prelude hiding (writeFile,readFile,putStrLn,getContents)
import Data.Text.IO (readFile,getContents) 
import Data.Text.Lazy.IO (writeFile,putStrLn)
import Data.Text (pack,Text)
import System.Console.CmdArgs

data Opts = Opts {
      size::(Int,Int) -- "window" size for calculating presentation coordinates 
    , input::String   -- Input file - defaults to stdin
    , output::String  -- Output file - defaults to stdout
    , title::String   -- Presentation title
    , include::String -- CSS and JS files to directly include in the presentation.
                      -- CSS is placed in the head, JS as the last elements of the body.
    , link::String    -- CSS and JS files to link to
    , watch::Bool     -- If set, himpress watches the input file and includes using inotify, and recompiles on changes.
    , nodefault::Bool -- If set, doesn't include the default CSS and JS files.
    , bare::Bool      -- If set, himpress will only generate the markup for slides, without a full document.
    } deriving (Show,Data,Typeable)

opts = Opts {
         size    = (1000,1000) &= help "Default size (in pixels) for each step"
       , input   = "" &= typFile
       , output  = "" &= typFile
       , title   = "" &= help "Presentation title"
       , include = "" &= typ listType &= help "CSS and JS files to include in the presentation - path from current directory."
       , link    = "" &= typ listType &= help "CSS and JS files to link to from the presentation"
       , bare    = False &= help "Only generate markup for slides, and not the full document"
       , nodefault = False &= help "Don't include the default CSS and JS"
       , watch     = False &= help "Recompile presentation on changes"
       } &= summary "himpress v1.0, (c) Matthew Sorensen 2012"

listType="<space-separated list of paths>"
          
--- Overall interface this file spits out:
format::Opts->IO (IO F.Format)
format o = fmap (dynamicOptions $ staticOptions o) $ includes o

dependencies::Opts->[FilePath]
dependencies o = if null $ input o
                 then words $ include o
                 else input o : words (include o)
watchFiles::[FilePath]->IO () ->IO ()
watchFiles files act = withINotify $ \inot -> mapM_ (watch inot) files >> loop
    where watch inot f = addWatch inot [CloseWrite] f $ const act 
          loop = threadDelay 1000000 >> loop
-- The assumption is made that you're an idiot (read: "I'm lazy") if this tool is 
-- set up to watch a file that it potentially modifies.
outputStream o | output o == "" = putStrLn
               | otherwise      = writeFile $ output o
inputStream::Opts->IO Text
inputStream o = case (input o,watch o) of
                  ("",True) -> error "An input file must be provided if --watch is used."
                  ("",False)-> getContents
                  (f,_)     -> readFile f
splitCssJs = (hasExt ".css" &&& hasExt ".js")  . words 
    where hasExt e = filter $ (e==) . takeExtension
staticOptions o = let (css,js) = splitCssJs $ link  o
                      plinks = map $ F.Link . pack
                  in F.Format {
                           F.meta = []
                         , F.title = pack $ title o
                         , F.bare = bare o
                         , F.scripts = plinks js
                         , F.style = plinks css
                         }
dynamicOptions::F.Format->([FilePath],[FilePath])->IO F.Format
dynamicOptions base (css,js) = do
  css <- mapM readFile css
  js  <- mapM readFile js
  return base { F.scripts =  F.scripts base ++ map F.Inline js ,
                F.style = F.style base ++ map F.Inline css}
includes::Opts->IO ([FilePath],[FilePath])
includes o
    | nodefault o = return $ splitCssJs $ include o
    | otherwise   = do
  imp <- getDataFileName "js/impress.min.js" 
  start <- getDataFileName "js/start.js"
  sty <- getDataFileName "css/style.min.css"
  return $ ((sty:) *** ([imp,start]++)) $ splitCssJs $ include o
