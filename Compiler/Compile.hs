{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Compiler.Compile (compile)where

import Compiler.Transitions
import Data.Lenses (fetch)
import Data.Monoid
import Data.List (mapAccumL)
import Data.Text (Text,pack)
import Data.Map (fromList)

-- Protoslides - ie. slides with a bunch of text and un-composed transitions.
type PSlide = ([Text],[Transition])
emptyPSlide = ([],[])

splitIntoPSlides::[Element]->[PSlide]
splitIntoPSlides (l:ls) = out $ foldl split' (l,emptyPSlide `addToTuple` l,[]) ls
    where split' (prev,slide,acc) new
              | fusible prev new = (new, addToTuple slide new, acc)
              | otherwise = (new, emptyPSlide `addToTuple` new, reverseTuple slide : acc)
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
          fusible (Right _) (Right t) = composes t
          composes = either snd snd
-- Now we may compose all of the transitions on each slide, and get a set of
-- native attributes + a presentation state for each slide
-- This is then converted to a set of native attributes, and sent off for formating.
compose::(Int,Int)->[Transition]->(Native,PState)
compose size = foldl append (mempty,mempty)
    where append st = either (combineNative st . fst) (combineChange st . fst)
          combineNative (nat,st) new = (nat `mappend` new,st)
          combineChange (nat,st) new = (nat, updateState size new st)

toNative::PState->Native
toNative p = Native {classes = mempty, attrs = fromList [
                                ("data-scale", str scale p)
                               ,("data-x", str x p)
                               ,("data-y", str y p)
                               ]}
    where str lens = pack . show . flip fetch lens

type Slide = (Native,Text)

compile::(Int,Int)->[Element]->[Slide]
compile size = snd . mapAccumL buildSlide mempty . splitIntoPSlides
    where buildSlide st (body,trans) = let (nat,pstate) = compose size trans
                                           st' = st `mappend` pstate
                                       in (st',(nat `mappend` toNative st', mconcat body))


-- Critical things to do:
-- Step to convert all relative movements to absolute coordinates
-- Step to compose all composable transitions that aren't separated by anything
-- Step to turn reduce every (composed) transition to a set of attributes and classes
-- (perhaps do this at the same time as composing them)
-- Then turn the list of Either Transition Element to [(Transition,[Element])]
-- Then format, which is easy enough.