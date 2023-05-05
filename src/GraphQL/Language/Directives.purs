-- This module defines types of *Directives* in GraphQL documents.
-- See: https://spec.graphql.org/June2018/#sec-Language.Directives
module GraphQL.Language.Directives
  ( ConstDirective
  , Directive
  , Directive'
  , Directives
  , Directives'(..)
  , unconst
  , unconsts
  ) where

import Prelude

import Data.Function (on)
import Data.Newtype (class Newtype, unwrap)
import Data.Set as Set
import GraphQL.Language.Arguments (Arguments')
import GraphQL.Language.Arguments as Args
import GraphQL.Language.Name (Name)

newtype Directives' const = Directives (Array (Directive' const))

derive instance newtypeDirectives :: Newtype (Directives' const) _

instance eqDirectives :: Ord const => Eq (Directives' const) where
  eq = eq `on` (unwrap >>> Set.fromFoldable)

instance ordDirectives :: Ord const => Ord (Directives' const) where
  compare = compare `on` (unwrap >>> Set.fromFoldable)

instance showDirectives :: Show const => Show (Directives' const) where
  show (Directives directives) = "(Directives" <> show directives <> ")"

type Directives = Directives' Unit

type ConstDirectives = Directives' Void

type Directive' const =
  { name :: Name
  , arguments :: Arguments' const
  }

type Directive = Directive' Unit

type ConstDirective = Directive' Void

unconst :: forall const. ConstDirective -> Directive' const
unconst { name, arguments } = { name, arguments: Args.unconsts arguments }

unconsts :: forall const. ConstDirectives -> Directives' const
unconsts (Directives directives) = Directives (unconst <$> directives)