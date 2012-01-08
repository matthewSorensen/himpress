{-# LANGUAGE OverloadedStrings #-}
import Himpress.Framework
import Himpress.Compile
import Himpress.Format
import Himpress.Modes

import System (getArgs)
import Prelude hiding (writeFile,readFile)
import Data.Text.IO (readFile) 
import Data.Text.Lazy.IO (writeFile)
import Text.Blaze.Renderer.Text (renderHtml)

generatePresentation = renderHtml . format . comp . parse
    where format = formatPresentation defaultOpts
          comp   = compile (1000,1000)
          parse  = parsePresentation defaultMode allModes
main = do
  (input:output:_) <- getArgs
  readFile input >>= writeFile output . generatePresentation