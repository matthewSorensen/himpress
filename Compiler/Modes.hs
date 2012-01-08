{-# LANGUAGE OverloadedStrings #-}
module Compiler.Modes 
    (defaultMode,allModes)
        where
import Compiler.Transitions
import Compiler.Framework

import Compiler.Modes.Text
import Compiler.Modes.Haskell
import Compiler.Modes.Spatial
import Compiler.Modes.Transitions

defaultMode = markdown
allModes = concat [textModes,haskellModes,spatialModes,transitionsModes]
