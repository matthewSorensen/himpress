{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Compiler.Modes.Spatial
    (spatialModes)
        where
import Compiler.Framework
import Compiler.Transitions

import Data.Attoparsec.Text hiding (D)
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Map (singleton)
import Data.Text (pack)
import Data.Monoid

spatialModes = [move,zoom,rotate]

-- move (up | down | left | right | integer integer)
move = Mode {name = "move", parser = parseDirection, format = Left (Right . Right . (,True) . Move)}
    where parseDirection = skipSpace *> (choice (map mkParser directions) <|> coords <|> pure R)
          directions = [(L,"left"),(R,"right"),(U,"up"),(D,"down")]
          mkParser (dir,str) = dir <$ string str
          coords = ((Coord .). (,)) <$> signed decimal <*> (skipSpace *> signed decimal)
-- zoom (in | out | integer)
zoom = Mode {name = "zoom", parser = parseMag, format = Left (Right . Right . (,True) . Scale)}
    where parseMag = skipSpace *> (zin <|> zout <|> signed decimal <|> pure (-1))
          zin  = -1 <$ string "in"
          zout =  1 <$ string "out"
-- rotate integer
rotate = Mode { name = "rotate", parser = skipSpace *> signed decimal, format = Left (Right . Left . (,True) . native)}
    where native angle = mempty {attrs = singleton "data-rotate" (pack $ show angle)}
