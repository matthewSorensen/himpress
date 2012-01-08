{-# LANGUAGE OverloadedStrings #-}
module Himpress.Modes 
    (defaultMode,allModes)
        where
import Himpress.Transitions
import Himpress.Framework

import Himpress.Modes.Text
import Himpress.Modes.Haskell
import Himpress.Modes.Spatial
import Himpress.Modes.Transitions

defaultMode = markdown
allModes = concat [textModes,haskellModes,spatialModes,transitionsModes]
