{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Language
import Data.Text
import Data.Char
import Data.Map as M
import Data.Attoparsec.Text as Atto
import Control.Monad

parse :: Text -> Either String (Module Text)
parse text = do
    bindings <- parseOnly (many' binding) text
    bindings' <- 
        let f m (name, value) = do
                when (member name m)
                    (error $ "Second definition of symbol \"" 
                        ++ unpack name ++ "\"")
                pure $ insert name value m
        in foldM f M.empty bindings
    pure $ Module $ fromList bindings

binding :: Parser (Text, Expression Text)
binding = do
    name <- identifier 
    skipSpaces
    string "="
    skipSpaces
    value <- expression
    skipSpaces
    pure (name, value)

expression :: Parser (Expression Text)
expression = choice 
    [ Literal <$> literal
    , Identifier <$> identifier 
    , do
        string "(\\"
        skipSpaces
        parameter <- identifier
        skipSpaces
        string "->"
        skipSpaces
        body <- expression
        string ")"
        pure $ Lambda parameter body
    , do
        string "("
        left <- expression 
        skipSpaces
        right <- expression
        string ")"
        pure $ Application left right
    ] 

literal :: Parser (Either Text Int)
literal = eitherP text integer
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
