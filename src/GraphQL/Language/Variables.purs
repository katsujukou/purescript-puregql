-- This module defines types of *Variables* in GraphQL documents.
-- See: https://spec.graphql.org/June2018/#sec-Language.Variables
module GraphQL.Language.Variables
  ( DefaultValue
  , Variable
  , VariableDefinition
  , VariableDefinitions
  , namePart
  , print
  , toVariable
  ) where

import Prelude

import Data.Maybe (Maybe)
import GraphQL.Language.InputValue (ConstValue)
import GraphQL.Language.Name (Name)
import GraphQL.Language.Name as Name
import GraphQL.Language.TypeReferences as TypeRef
import Safe.Coerce (coerce)

newtype Variable = Variable Name

derive newtype instance eqVariable :: Eq Variable
derive newtype instance ordVariable :: Ord Variable

instance showVariable :: Show Variable where
  show (Variable n) = "(Variable " <> Name.toString n <> ")"

toVariable :: Name -> Variable
toVariable = Variable

namePart :: Variable -> Name
namePart (Variable n) = n

print :: Variable -> String
print = ("$" <> _) <<< Name.toString <<< coerce

type VariableDefinitions = Array VariableDefinition

type VariableDefinition =
  { type :: TypeRef.Type
  , default :: Maybe DefaultValue
  }

type DefaultValue = ConstValue
