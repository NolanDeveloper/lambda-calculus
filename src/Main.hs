{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Set(Set) 
import qualified Data.Set as Set

import Paths_lambda_calculus

import Language hiding (Expression)
import qualified Language(Expression)
import Parser

type Name = Text
type Expression = Language.Expression Name

readDataFile :: String -> IO Text
readDataFile fileName = getDataFileName fileName >>= Text.readFile 

handleError :: Either String a -> IO a
handleError (Left message) = error message
handleError (Right value) = pure value

data ExecutionState
    = ExecutionState {
        bindings :: Bindings Name,
        unique :: Int
    }

type Execution a = StateT ExecutionState IO a

freeVariables :: Expression -> Set Name
freeVariables (Identifier name) = Set.singleton name
freeVariables (Application function argument) = Set.union function' argument'
  where
    function' = freeVariables function
    argument' = freeVariables argument
freeVariables (Lambda parameter body) = Set.delete parameter (freeVariables body)

boundVariables :: Expression -> Set Name
boundVariables (Identifier _) = Set.empty
boundVariables (Application function argument) = Set.union function' argument'
  where
    function' = boundVariables function
    argument' = boundVariables argument
boundVariables (Lambda parameter body) = Set.insert parameter (boundVariables body)

makeNewName :: Name -> Execution Name
makeNewName oldName = do
    state@ExecutionState { unique = n } <- get
    put $ state { unique = n + 1 }
    pure $ oldName <> Text.pack (show n)

rename :: Name -> Name -> Expression -> Expression 
rename oldName newName expression = go expression
  where 
    go expression@(Identifier name)
        | name == oldName = (Identifier newName)
        | otherwise = expression
    go (Application function argument) = Application (go function) (go argument)
    go expression@(Lambda parameter body)
        | parameter /= oldName = Lambda parameter (go body)
        | otherwise = expression

renameAll :: Set Name -> Expression -> Execution Expression
renameAll variables expression = go expression
  where
    go expression@(Identifier _) = pure expression
    go (Application function argument) = Application <$> go function <*> go argument
    go (Lambda parameter body)
        | Set.member parameter variables = do
            newName <- makeNewName parameter
            let newBody = rename parameter newName body
            pure $ Lambda newName newBody
        | otherwise = Lambda parameter <$> go body

substitute :: Name -> Expression -> Expression -> Execution Expression
substitute parameter value body = do
    body <- renameAll problemVariables body
    go body
  where
    problemVariables = Set.intersection (freeVariables value) (boundVariables body)
    go expression@(Identifier name)
        | name == parameter = pure $ value
        | otherwise = pure $ expression
    go (Application function argument) = Application <$> go function <*> go argument
    go expression@(Lambda parameter' body')
        | parameter == parameter' = pure $ expression
        | otherwise = Lambda parameter' <$> go body'

evaluateSome :: Expression -> Execution (Maybe Expression)
evaluateSome expression = go Set.empty expression
  where
    go :: Set Name -> Expression -> Execution (Maybe Expression)
    go boundVariables (Identifier name) 
        | Set.member name boundVariables = pure Nothing
        | otherwise = Just . lookupName name . bindings <$> get
    go _ (Application (Lambda parameter body) argument) 
        = Just <$> substitute parameter argument body
    go boundVariables (Application function value) = do
        maybeFunction <- go boundVariables function
        case maybeFunction of
          Nothing -> (Application function <$>) <$> go boundVariables value
          Just function' -> pure $ Just $ Application function' value
    go boundVariables (Lambda parameter body) = (Lambda parameter <$>) <$> body'
      where
        body' = go (Set.insert parameter boundVariables) body

main :: IO ()
main = do
    content <- readDataFile "data/source.txt"
    bindings <- handleError $ parse content
    display bindings
    evalStateT (go (Identifier "main")) (ExecutionState bindings 0)
  where
    go expression = do
        liftIO $ display expression
        maybeNextExpression <- evaluateSome expression
        case maybeNextExpression of
          Nothing -> pure ()
          Just nextExpression -> go nextExpression
