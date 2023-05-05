-- This module defines types representing *arguments* in GraphQL documents.
-- see: https://spec.graphql.org/June2018/#sec-Language.Arguments
module GraphQL.Language.Arguments
  ( Argument
  , Argument'
  , Arguments
  , Arguments'(..)
  , ConstArgument
  , ConstArguments
  , empty
  , unconst
  , unconsts
  ) where

import Prelude

import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import GraphQL.Language.InputValue (Value')
import GraphQL.Language.InputValue as InputValue
import GraphQL.Language.Name (Name)

newtype Arguments' const = Arguments (Array (Argument' const))

derive instance Newtype (Arguments' const) _

instance eqArguments :: Ord const => Eq (Arguments' const) where
  eq = eq `on` (unwrap >>> Set.fromFoldable)

instance ordArguments :: Ord const => Ord (Arguments' const) where
  compare = compare `on` (unwrap >>> Set.fromFoldable)

instance showArguments :: Show const => Show (Arguments' const) where
  show (Arguments args) = "(Arguments " <> show args <> ")"

type Arguments = Arguments' Unit

type ConstArguments = Arguments' Void

type Argument' const =
  { name :: Name
  , value :: Value' const
  }

type Argument = Argument' Unit

type ConstArgument = Argument' Void

empty :: forall const. Arguments' const
empty = Arguments []

unconst :: forall const. ConstArgument -> Argument' const
unconst { name, value } = { name, value: InputValue.unconst value }

unconsts :: forall const. ConstArguments -> Arguments' const
unconsts (Arguments args) = Arguments (unconst <$> args)