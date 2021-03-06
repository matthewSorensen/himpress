{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Himpress.Modes.Spatial
    (spatialModes)
        where
import Himpress.Framework
import Himpress.Transitions

import Data.Attoparsec.Text hiding (D)
import Data.Attoparsec.Combinator
import Control.Applicative
import Data.Map (singleton,insert)
import Data.Text (pack)
import Data.Monoid

spatialModes = [move,zoom,rotate]

-- move (up | down | left | right | integer integer integer)
move = Mode {name = "move", parser = parseDirection, format = Left (Right . Right . (,True) . Move)}
    where parseDirection = skipSpace *> (choice (map mkParser directions) <|> coords <|> pure R)
          directions = [(L,"left"),(R,"right"),(U,"up"),(D,"down")]
          mkParser (dir,str) = dir <$ string str
          coords = Coord <$> parseTriple
          
parseTriple = comb <$> spaceInt <*> spaceInt <*> spaceInt
    where comb a b c = (a,b,c)
          spaceInt = skipSpace *> signed decimal

-- zoom (in | out | float)
zoom = Mode {name = "zoom", parser = parseMag, format = Left (Right . Right . (,True) . Scale)}
    where parseMag = skipSpace *> (zin <|> zout <|> signed rational <|> pure (-1))
          zin  = -1 <$ string "in"
          zout =  1 <$ string "out"
-- rotate integer
-- rotate integer integer
-- rotate integer integer integer
rotate = Mode { name = "rotate", 
                parser = skipSpace *> signed decimal >>= parse3 . pack . show, 
                format = Left (Right . Left . (,True) . native)}
    where native set = mempty {attrs = set}
          parse3 i = (singleton "data-rotate" i <$ (skipSpace *> endOfInput)) <|> higher (singleton "data-rotate-x" i)
          higher x = addDim "data-rotate-y" x >>= addDim "data-rotate-z"
          addDim attr set = skipSpace *> ((set <$ endOfInput) <|> (flip (insert attr) set . pack . show <$> signed decimal))
          