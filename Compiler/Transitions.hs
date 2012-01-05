module Compiler.Transitions where

import Data.Text

data Transition = Transition {
      classes::[(Text,Text)],
      dataAttrs::[(Text,Text)]
    } | Move Direction
                deriving(Show,Eq)

data Direction = L | R | D | U | Coord (Int,Int)
                 deriving(Show,Eq)