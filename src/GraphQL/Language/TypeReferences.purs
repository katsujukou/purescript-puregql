module GraphQL.Language.TypeReferences where

import Prelude
import Prim hiding (Type)

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import GraphQL.Language.Name (Name)
import GraphQL.Language.Name as Name

data Type
  = TypNamed NamedType
  | TypList ListType
  | TypNonNull NonNullType

derive instance eqType :: Eq Type
derive instance ordType :: Ord Type
derive instance genericType :: Generic Type _
instance showType :: Show Type where
  show typ = genericShow typ

newtype NamedType = NamedType Name

derive newtype instance eqNamedType :: Eq NamedType
derive newtype instance ordNameType :: Ord NamedType
derive instance genericNamedType :: Generic NamedType _
derive instance newtypeName :: Newtype NamedType _

instance showNamedType :: Show NamedType where
  show (NamedType n) = "(NamedType " <> Name.toString n <> ")"

type ListType =
  { of :: Type
  }

type NonNullType =
  { of :: Type
  }