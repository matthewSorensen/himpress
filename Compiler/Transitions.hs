module Compiler.Transitions where

import Data.Text

data Transition = Transition {
      classes::[(Text,Text)],
      dataAttrs::[(Text,Text)],
      _compose::Bool
    } | Move Direction Bool
                deriving(Show,Eq)

data Direction = L | R | D | U | Coord (Int,Int)
                 deriving(Show,Eq)

composable::Transition->Bool
composable (Transition _ _ t) = t
composable (Move _ t)         = t

