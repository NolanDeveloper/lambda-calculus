module Renamer where

import Language
import Data.Text
import Control.Monad
import Control.Monad.State
import Data.Map as M
import Data.Function
import Data.Maybe

import Debug.Trace

data Variable = Variable Int Text

instance Show Variable where
    show (Variable unique name) = unpack name ++ show unique

instance Eq Variable where
    (Variable left _) == (Variable right _) = left == right

instance Ord Variable where
    (Variable left _) <= (Variable right _) = left <= right

data RenamerState
    = RenamerState {
        unique :: Int,
        scope :: Map Text Variable
    }

type Renamer a = State RenamerState a

rename :: Module Text -> Module Variable
rename module_ = evalState action initialState
  where
    action = Module <$> renamer (moduleBindings module_)
    initialState 
        = RenamerState {
            unique = 0,
            scope = M.empty
        }

renamer
    :: Map Text (Expression Text) 
    -> Renamer (Map Variable (Expression Variable))
renamer bindings = do
    bindings' <- renameLeftSide bindings 
    renameRightSide bindings'

nextUnique :: Renamer Int
nextUnique = do
    state@RenamerState { unique = n } <- get
    put $ state { unique = n + 1 }
    pure n

makeVariable :: Text -> Renamer Variable
makeVariable name = Variable <$> nextUnique <*> pure name

bringToScope :: Variable -> Renamer ()
bringToScope variable@(Variable _ name) = do
    state@RenamerState { scope = scope } <- get
    put $ state { scope = insert name variable scope }

deleteFromScope :: Variable -> Renamer ()
deleteFromScope variable@(Variable _ name) = do
    state@RenamerState { scope = scope } <- get
    put $ state { scope = delete name scope }

renameLeftSide 
    :: Map Text (Expression Text) 
    -> Renamer (Map Variable (Expression Text))
renameLeftSide bindings = do
    bindings' <- 
        forM (toList bindings) $ \(name, value) -> do
            variable <- makeVariable name
            bringToScope variable
            pure (variable, value)
    pure $ fromAscList bindings'

renameRightSide     
    :: Map Variable (Expression Text) 
    -> Renamer (Map Variable (Expression Variable))
renameRightSide bindings = traverse renameExpression bindings

lookup :: Text -> Renamer (Maybe Variable)
lookup name = (!? name) . scope <$> get

renameExpression 
    :: Expression Text
    -> Renamer (Expression Variable)

renameExpression (Identifier name) = do
    maybeVariable <- Renamer.lookup name
    case Identifier <$> maybeVariable of
        Nothing -> error $ "No such name: \"" ++ unpack name ++ "\""
        Just p -> pure p

renameExpression (Literal value) = pure $ Literal value

renameExpression (Application function argument) = 
    let function' = renameExpression function
        argument' = renameExpression argument
    in Application <$> function' <*> argument'

renameExpression (Lambda parameterName body) = do
    maybeVariable <- Renamer.lookup parameterName
    parameterVariable <- makeVariable parameterName
    bringToScope parameterVariable
    newBody <- renameExpression body
    case maybeVariable of
        Nothing -> deleteFromScope parameterVariable
        Just variable -> bringToScope variable
    pure $ Lambda parameterVariable newBody
