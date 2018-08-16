{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad
import Data.Map

import Paths_lambda_calculus

import Language
import Parser
import Renamer

readDataFile :: String -> IO Text
readDataFile fileName = getDataFileName fileName >>= T.readFile 

main :: IO ()
main = do
    content <- readDataFile "data/source.txt"
    let onError error = do
            putStr "Error: " 
            putStrLn error
    either onError id $ do
        sourceModule@(Module bindings) <- parse content
        let renamedModule@(Module bindings') = rename sourceModule
        let display name value = do
                putStr $ show name
                putStr " = "
                putStrLn $ show value
        pure $ do
            putStrLn "===================================="
            traverseWithKey display bindings
            putStrLn "===================================="
            traverseWithKey display bindings'
            putStrLn "===================================="
