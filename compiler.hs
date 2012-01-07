{-# LANGUAGE OverloadedStrings #-}
import Compiler.Framework
import Compiler.Compile
import Compiler.Format


import System (getArgs)
import Prelude hiding (writeFile,readFile)
import Data.Text.IO (readFile) 
import Data.ByteString.Lazy (writeFile)
import Text.Blaze.Renderer.Utf8 (renderHtml)


import Compiler.Transitions

literal = Mode {name = "literal", parser = return (), format = Right $ const $ Left}

next = Mode {name = "next", parser = return (), format = Left fmt}
    where fmt = const $ Right $ Right (Move L,False)

generatePresentation = renderHtml . format . comp . parse
    where format = formatPresentation defaultOpts
          comp   = compile (1000,1000)
          parse  = parsePresentation literal [literal,next]

main = do
  (input:output:_) <- getArgs
  readFile input >>= writeFile output . generatePresentation