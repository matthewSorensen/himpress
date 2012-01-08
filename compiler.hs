{-# LANGUAGE OverloadedStrings #-}
import Compiler.Framework
import Compiler.Compile
import Compiler.Format
import Compiler.Modes

import System (getArgs)
import Prelude hiding (writeFile,readFile)
import Data.Text.IO (readFile) 
import Data.ByteString.Lazy (writeFile)
import Text.Blaze.Renderer.Utf8 (renderHtml)


generatePresentation = renderHtml . format . comp . parse
    where format = formatPresentation defaultOpts
          comp   = compile (1000,1000)
          parse  = parsePresentation defaultMode allModes

main = do
  (input:output:_) <- getArgs
  readFile input >>= writeFile output . generatePresentation