{-# LANGUAGE TupleSections #-}
module Compiler.Compile where

import Compiler.Transitions
import Data.Text (Text)


type Slide = ([Text],[Transition])
emptySlide = ([],[])

splitIntoSlides::[Element]->[Slide]
splitIntoSlides l = out $ foldl split' (head l,emptySlide,[]) $ tail l
    where split' (prev,slide,acc) new
              | fusible prev new = (new, addToTuple slide new, acc)
              | otherwise = (new, emptySlide, reverseTuple slide : acc)
          out (_,_,x) = reverse x
          reverseTuple (a,b) = (reverse a,reverse b)
          addToTuple (a,b) = either ((,b) . (:a)) ((a,) . (:b))
          -- Text , Text are on the same slide
          -- Text, Transition are never
          -- Transition, Transition are, iff the second is composable
          -- Transition, Text are always
          fusible (Left  _) (Left  _) = True
          fusible (Left  _) (Right _) = False
          fusible (Right _) (Left  _) = True
          fusible (Right _) (Right (Transition _ b)) = b

-- Now we may compose all of the transitions on each slide, and get a set of
-- native attributes + a presentation state for each slide
-- This is then converted to a set of native attributes, and sent off for formating.
