{-# LANGUAGE TemplateHaskell, FlexibleContexts #-}
module Compiler.Transitions where

import Data.Text
import Data.Map (Map)
import Data.Lenses.Template

type Element  = Either Text Transition

data Transition = Transition Change Bool
                  deriving(Show,Eq)

data Change = Rotation Int | Move Direction | Scale Int
            deriving(Show,Eq)

data Direction = L | R | D | U | Coord (Int,Int)
                 deriving(Show,Eq)

data Native = Native {classes_::[Text],attrs_::Map Text Text}
              deriving(Show,Eq)
$(deriveLenses ''Native)

data PState = PState {
      x_::Int,
      y_::Int,
      theta_::Int,
      scale_::Int
    } deriving (Show,Eq)

$(deriveLenses ''PState)
