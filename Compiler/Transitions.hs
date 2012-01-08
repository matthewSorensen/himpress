{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Compiler.Transitions where

import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set,singleton)
import Data.Monoid
import Data.Lenses.Template
import Data.Lenses (alter)

type Slide = (Native,Text)

type Element  = Either Text Transition
type Transition = Either (Native,Bool) (Change,Bool)

data Change = Move Direction | Scale Int
            deriving(Show,Eq)

data Direction = L | R | D | U | Coord (Int,Int)
                 deriving(Show,Eq)

data Native = Native {classes::Set Text,attrs::Map Text Text}
            deriving(Show,Eq)

instance Monoid Native where
    mempty = Native {classes = singleton "step", attrs = mempty}
    (Native a b) `mappend` (Native c d) = Native (a `mappend` c) (b `mappend` d)

data PState = PState {
      x_::Int,
      y_::Int,
      scale_::Int
    } deriving (Show,Eq)

$(deriveLenses ''PState)

instance Monoid PState where
    mempty = PState 0 0 0
    (PState a b c) `mappend` (PState d e f) = PState (a+d) (b+e) (c+f)

startPState = PState 0 0 1

updateState::(Int,Int)->Change->PState->PState
updateState _ (Scale i)  = alter scale (+i)
updateState size (Move dir) = alter x (+dx) . alter y (+dy)
    where (dx,dy) = sizeOf size dir

sizeOf::(Int,Int)->Direction->(Int,Int)
sizeOf _ (Coord i) = i
sizeOf (x,_) L     = (-x,0)
sizeOf (x,_) R     = ( x,0)
sizeOf (_,y) D     = (0,-y)
sizeOf (_,y) U     = (0,y)
