{-# LANGUAGE OverloadedStrings #-}
module Compiler.Modes 
    (defaultMode,allModes)
        where

import Compiler.Transitions
import Compiler.Framework
import Compiler.Modes.Text


defaultMode = markdown
allModes = concat [textModes,[next]]


next = Mode {name = "next", parser = return (), format = Left fmt}
    where fmt = const $ Right $ Right (Move R,False)
