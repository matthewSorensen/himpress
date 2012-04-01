{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
import Himpress.Framework (parsePresentation)
import Himpress.Compile
import qualified Himpress.Format as F
import Himpress.Modes
import Himpress.Plumbing

import Control.Applicative

import System (getArgs)
import System.Console.CmdArgs
import Text.Blaze.Renderer.Text (renderHtml)




generate size fopts = renderHtml . F.formatPresentation fopts . compile size . parse
    where parse  = parsePresentation defaultMode allModes

main = do
  o <- cmdArgs opts
  formatOpts <- format o
  let source = inputStream o
  let sink   = outputStream o
  watchOrNot o $ (generate (size o) <$> formatOpts <*> source) >>= sink


watchOrNot::Opts->IO () -> IO ()
watchOrNot o act | watch o = watchFiles (dependencies o) act
                 | otherwise = act
