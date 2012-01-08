{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Compiler.Framework 
    (Element,Format,DocMode (..),parsePresentation)
        where

import Prelude hiding (takeWhile,lookup)
import Data.Char
import Compiler.Transitions
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Char (isSpace)
import Data.Map (Map,lookup,fromList)
import Control.Applicative

type Text = T.Text

type Format a = Either (a->Element) (a->Text->Element)

data DocMode = forall a . Mode {
      name::Text,
      parser::Parser a,
      format::Format a
    }

runParser::Parser a->Text->a
runParser p t = either (flail t) id $ parseOnly p t
    where flail t err = error $ concat ["Parser failed with error '",err,"' on text ",show t]

apply::DocMode->Text->Text->(Element,Maybe Text)
apply (Mode _ parse form) opts text = either l r form
    where parsed = runParser parse opts
          l    f = (f parsed, Just text)
          r    f = (f parsed text, Nothing)

-- Commands take the form >>> {identifier} {rest of the line is passed to the DocMode}\n
-- All of the text up until the next start of a tag
text::Parser Text
text = T.concat . reverse <$> chunks []
    where chunks acc = scan (0::Int) stateMachine >>= post acc
          stateMachine 4  _   = Nothing
          stateMachine 0 '\\' = Just 1
          stateMachine 0 '>'  = Just 2
          stateMachine n '>'  = Just $ n + 1
          stateMachine _ _    = Just 0
          post acc chunk = case T.splitAt (T.length chunk - 4) chunk of
                             (c,"\\>>>") -> chunks $ ">>>":c:acc
                             (c,x) -> return $ headOf x: c : acc
          headOf x 
              | T.drop 1 x /= ">>>" = if x /= ">>>" then x else ""
              | otherwise           = T.take 1 x

-- Parses the body of a command, returning the name and options
command::Parser (Text,Text)
command = whitespace *> ((,) <$> name <*> rest) <* endOfLine
    where whitespace = skipWhile (\c -> isSpace c && c /= '\n')
          name = takeWhile $ inClass "a-zA-Z0-9-"
          rest = takeTill (== '\n')
-- This is safe to use at the end of a file
pair::DocMode->Map Text DocMode->Parser [Element]
pair def modes = (uncurry normalize .) . combine <$> command <*> text
    where combine (name,args) = apply (getMode name) args  
          getMode m     =  maybe (error $ "Couldn't find mode " ++ show m) id $ lookup m modes
          defaultMode old new  
               | T.all isSpace new = [old]
               | otherwise         = [old, fst $ apply def "" new]
          normalize old = maybe [old] (defaultMode old)

-- This is where the parser bug mentioned in issue 1 lies - the text should be checked to be not null or whitespace.
stream::DocMode->Map Text DocMode->Parser [Element]
stream def modes = combine <$> text <*> many (pair def modes) 
    where combine start rest = fst (apply def "" start) : concat rest

parsePresentation::DocMode->[DocMode]->Text->[Element]
parsePresentation def modes = flail . parseOnly (stream def $ mkModes modes)
    where flail = either (\err-> error $ "Failed to parse presentation, with error" ++ err) id
          mkModes = fromList . map (\m->(name m,m))
