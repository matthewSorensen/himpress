{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Himpress.Compile 
    (compile)
        where

import Himpress.Transitions
import Data.Lenses (fetch)
import Data.Monoid
import Data.List (mapAccumL)
import Data.Text (pack)
import Data.Map (fromList)
import Text.Blaze (Html,toHtml)

-- Protoslides - ie. slides with a bunch of text and un-composed transitions.
type PSlide = ([Html],[Transition])
emptyPSlide = ([],[])

splitIntoPSlides::[Element]->[PSlide]
splitIntoPSlides []     = []
splitIntoPSlides (l:ls) = out $ foldl split' (l,emptyPSlide `addToTuple` l,[]) ls
    where split' (prev,slide,acc) new
              | fusible prev new = (new, addToTuple slide new, acc)
              | otherwise = (new, emptyPSlide `addToTuple` new, reverseTuple slide : acc)
          -- Reverse the last slide, then the entire presentation.
          out (_,l,x) = reverse $ reverseTuple l :x
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
                               ,("data-z", str z p)
                               ]}
    where str lens = pack . show . flip fetch lens

compile::(Int,Int)->[Element]->[Slide]
compile size = snd . mapAccumL buildSlide mempty . splitIntoPSlides
    where buildSlide st (body,trans) = let (nat,pstate) = compose size trans
                                           st' = st `mappend` pstate
                                       in (st',(nat `mappend` toNative st', toHtml body))
