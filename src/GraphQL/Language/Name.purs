module GraphQL.Language.Name where

import Prelude

newtype Name = Name String

derive newtype instance eqName :: Eq Name
derive newtype instance ordName :: Ord Name
instance showName :: Show Name where
  show (Name n) = "(Name " <> n <> ")"

toString :: Name -> String
toString (Name n) = n
