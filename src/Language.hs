{-# LANGUAGE OverloadedStrings #-}
module Language where

import Data.Text(Text)
import qualified Data.Text as Text
import Data.Map(Map)
import qualified Data.Map as Map

data GenericBindings a b = Bindings (Map a (Expression b))

type Bindings a = GenericBindings a a

lookupName :: Ord a => a -> GenericBindings a b -> Expression b
lookupName name (Bindings map) = map Map.! name

insertName :: Ord a => a -> Expression b -> GenericBindings a b -> GenericBindings a b
insertName name value (Bindings map) = Bindings $ Map.insert name value map

data Expression id
    = Identifier id
    | Application (Expression id) (Expression id)
    | Lambda id (Expression id)

class Pretty a where
    pretty :: a -> Text

display :: Pretty a => a -> IO ()
display x = putStrLn $ Text.unpack $ pretty x

instance Pretty Text where
    pretty = id

instance (Pretty a, Pretty b) => Pretty (GenericBindings a b) where
    pretty (Bindings bindings) = Map.foldMapWithKey prettyBinding bindings
      where
        prettyBinding name value = pretty name <> " = " <> pretty value <> "\n"

instance Pretty a => Pretty (Expression a) where
    pretty (Identifier name) = pretty name
    pretty (Application function argument) = "(" <> function' <> " " <> argument' <> ")"
      where
        function' = pretty function
        argument' = pretty argument
    pretty (Lambda parameter body) = "(\\" <> parameter' <> " -> " <> body' <> ")"
      where
        parameter' = pretty parameter
        body' = pretty body
