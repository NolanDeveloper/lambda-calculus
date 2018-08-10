{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Language
import Data.Text
import Data.Char
import Data.Attoparsec.Text as Atto

parse :: Text -> Either String [(Text, Expression)]
parse = eitherResult . Atto.parse parser 

parser :: Parser [(Text, Expression)]
parser = do
    skipSpaces
    name <- identifier 
    skipSpaces
    string "="
    skipSpaces
    value <- expression
    pure [(name, value)]

expression :: Parser Expression
expression = choice 
    [ Literal <$> literal
    , Identifier <$> identifier 
    ] 

literal :: Parser Literal
literal = choice 
    [ LText <$> text
    , LInteger <$> integer]
  where
    text = pack <$> wrapped quotes anyChar quotes
      where 
        quotes = string "\""
    integer = signed decimal

identifier :: Parser Text
identifier = do
    name <- many1 letter
    pure $ pack name

---------------
-- Helpers

skipSpaces :: Parser ()
skipSpaces = skipWhile isSpace

wrapped :: Parser a -> Parser b -> Parser c -> Parser [b]
wrapped begin content end = do
    begin
    manyTill content end
