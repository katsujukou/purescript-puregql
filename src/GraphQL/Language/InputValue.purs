-- This module defines types for representing *input values* in GraphQL documents.
-- see: https://spec.graphql.org/June2018/#sec-Input-Values
module GraphQL.Language.InputValue where

import Prelude

import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Show.Generic (genericShow)
import Data.String (joinWith)
import Data.Tuple.Nested (type (/\), (/\))
import GraphQL.Language.Name (Name)
import GraphQL.Language.Name as Name
import Unsafe.Coerce (unsafeCoerce)

-- Type of *Input Values* in GraphQL documents.
-- The type parameter `const` encodes "parametricity" described in the GraphQL specification.
-- `Value' Void` denotes those values defined as constant.
data Value' const
  = ValVariable const
  | ValInt Int
  | ValFloat Number
  | ValString String
  | ValBoolean Boolean
  | ValNull
  | ValEnum EnumValue
  | ValList (ListValue const)
  | ValObject (ObjectValue const)

derive instance eqValue :: Eq const => Eq (Value' const)
derive instance ordValue :: Ord const => Ord (Value' const)
derive instance genericValue :: Generic (Value' const) _
instance showValue :: Show const => Show (Value' const) where
  show = genericShow

type Value = Value' Unit

type ConstValue = Value' Void

newtype EnumValue = EnumValue Name

derive newtype instance eqEnumValue :: Eq EnumValue
derive newtype instance ordEnumValue :: Ord EnumValue
instance showEnumValue :: Show EnumValue where
  show (EnumValue n) = "(EnumValue " <> Name.toString n <> ")"

newtype ListValue const = ListValue (Array (Value' const))

derive newtype instance eqListValue :: Eq const => Eq (ListValue const)
derive newtype instance ordListValue :: Ord const => Ord (ListValue const)
derive instance Newtype (ListValue const) _

instance showListValue :: Show const => Show (ListValue const) where
  show (ListValue vs) = "(ListValue " <> show vs <> ")"

newtype ObjectValue const = ObjectValue (Array (Name /\ Value' const))

derive instance Newtype (ObjectValue const) _

instance eqObjectValue :: Eq const => Eq (ObjectValue const) where
  eq = eq `on` (unwrap >>> Map.fromFoldable)

instance ordObjectValue :: Ord const => Ord (ObjectValue const) where
  compare = compare `on` (unwrap >>> Map.fromFoldable)

instance showObjectValue :: Show const => Show (ObjectValue const) where
  show (ObjectValue vs) = "(ObjectValue " <> joinWith ", " (map showMember vs) <> ")"
    where
    showMember (n /\ val) = Name.toString n <> ":" <> show val

-- This function is not unsafe as it seems
unconst :: forall const. ConstValue -> Value' const
unconst = unsafeCoerce