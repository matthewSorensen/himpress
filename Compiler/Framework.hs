{-# LANGUAGE ExistentialQuantification, OverloadedStrings #-}
module Compiler.Framework where

import Prelude hiding (takeWhile,lookup)
import Data.Char
import Compiler.Transitions
import Data.Attoparsec.Text
import qualified Data.Text as T
import Data.Map (Map,lookup,fromList)
import Control.Applicative

type Text = T.Text

type Element  = Either Transition Text
type Format a = Either (a->Element) (a->Text->Element)

data DocMode = forall a . Mode {
      name::Text,
      parser::Parser a,
      format::Format a
    }

literal::DocMode 
literal = Mode {name = "literal", parser = pure (), format = Right fmt}
    where fmt = const Right
          
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
          headOf x = case T.head x of
                       '>' -> ""
                       x   -> T.singleton x
foo = takeText

-- Parses the body of a command, returning the name and options
command::Parser (Text,Text)
command = whitespace *> ((,) <$> name <*> rest) <* endOfLine
    where whitespace = skipWhile (\c -> isSpace c && c /= '\n')
          name = takeWhile $ inClass "a-zA-Z0-9-"
          rest = takeTill (== '\n')

-- This is moderately difficult.
-- Has lots of bugs in it.
parseStream::DocMode->Map Text DocMode->Parser [Element]
parseStream def modes =  (:) <$> start <*> (reverse <$> rest)
    where defMode = fst . apply def ""
          start =  defMode <$> text
          rest = (endOfInput *> pure []) <|> (pair <*> rest)
          pair = do
            (name,args) <- command
            let mode = maybe (error $ "Couldn't find mode " ++ show name) id $ lookup name modes
            (elem,remain) <- apply mode args <$> text
            pure $ maybe (elem:) (\x-> ((defMode x):) . (elem:)) remain

testStream = parseStream literal (fromList [("literal",literal)])

-- Parse text; if we're at the end of input fail
test p = runParser p . T.pack