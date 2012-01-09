{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleContexts #-}
module Himpress.Transitions where

import Data.Text (Text)
import Data.Map (Map)
import Data.Set (Set,singleton)
import Data.Monoid
import Data.Lenses.Template
import Data.Lenses (alter,fetch)
import Text.Blaze (Html)

type Slide = (Native,Html)

type Element  = Either  Html Transition
type Transition = Either (Native,Bool) (Change,Bool)

data Change = Move Direction | Scale Float
            deriving(Show,Eq)

data Direction = L | R | D | U | Coord (Int,Int,Int)
                 deriving(Show,Eq)

data Native = Native {classes::Set Text,attrs::Map Text Text}
            deriving(Show,Eq)

instance Monoid Native where
    mempty = Native {classes = singleton "step", attrs = mempty}
    (Native a b) `mappend` (Native c d) = Native (a `mappend` c) (b `mappend` d)

data PState = PState {
      x_::Int,
      y_::Int,
      z_::Int,
      scale_::Float
    } deriving (Show,Eq)

$(deriveLenses ''PState)

instance Monoid PState where
    mempty = PState 0 0 0 1
    (PState a b c d) `mappend` (PState e f j k) = PState (a+e) (b+f) (c+j) (d*k)

updateState::(Int,Int)->Change->PState->PState
updateState _ (Scale i)     = alter scale (*i) 
updateState size (Move dir) = alter z (+dz) . alter x (+dx) . alter y (+dy)
    where (dx,dy,dz) = sizeOf size dir

sizeOf::(Int,Int)->Direction->(Int,Int,Int)
sizeOf _ (Coord i) = i
sizeOf (x,_) L     = (-x,0,0)
sizeOf (x,_) R     = ( x,0,0)
sizeOf (_,y) D     = (0,y,0)
sizeOf (_,y) U     = (0,-y,0)
