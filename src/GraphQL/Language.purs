module GraphQL.Language where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import GraphQL.Language.Arguments (Arguments)
import GraphQL.Language.Directives (Directives)
import GraphQL.Language.Name (Name)
import GraphQL.Language.Name as Name
import GraphQL.Language.TypeReferences (NamedType)
import GraphQL.Language.Variables (VariableDefinitions)

type Document = Array Definition

data Definition
  = DefnExecutable ExecutableDefinition
  | DefnTypeSystem
  | DefnTypeSystemExtension

derive instance eqDefinition :: Eq Definition
derive instance ordDefinition :: Ord Definition
derive instance genericDefinition :: Generic Definition _
instance showDefinition :: Show Definition where
  show = genericShow

data ExecutableDefinition
  = ExcDefnOperation OperationDefinition
  | ExcDefnFragment FragmentDefinition

derive instance eqExecutableDefinition :: Eq ExecutableDefinition
derive instance ordExecutableDefinition :: Ord ExecutableDefinition
derive instance genericExecutableDefinition :: Generic ExecutableDefinition _
instance showExecutableDefinition :: Show ExecutableDefinition where
  show = genericShow

data OperationDefinition
  = OprDefnFull OperationType (Maybe Name) VariableDefinitions Directives SelectionSet
  | OprDefnShorthand SelectionSet

derive instance eqOperationDefinition :: Eq OperationDefinition
derive instance ordOperationDefinition :: Ord OperationDefinition
derive instance genericOperationDefinition :: Generic OperationDefinition _
instance howOperationDefinition :: Show OperationDefinition where
  show = genericShow

data OperationType = Query | Mutation | Subscription

derive instance eqOperationType :: Eq OperationType
derive instance ordOperationType :: Ord OperationType
derive instance genericOperationType :: Generic OperationType _

instance showOperationType :: Show OperationType where
  show = genericShow

type SelectionSet = Array Selection

data Selection
  = SelField Field
  | SelFragment FragmentSpread
  | SelInline InlineFragment

derive instance eqSelection :: Eq Selection
derive instance ordSelection :: Ord Selection
derive instance genericSelection :: Generic Selection _

instance showSelection :: Show Selection where
  show selection = genericShow selection

type Field =
  { alias :: Maybe FieldAlias
  , name :: Name
  , args :: Arguments
  , directives :: Directives
  , selects :: Maybe SelectionSet
  }

type FieldAlias = Name

type FragmentSpread =
  { name :: FragmentName
  , directives :: Directives
  }

type FragmentDefinition =
  { name :: FragmentName
  , typeCond :: TypeCondition
  , directives :: Directives
  , selects :: SelectionSet
  }

type TypeCondition =
  { on :: NamedType
  }

newtype FragmentName = FragmentName Name

derive newtype instance eqFragmentName :: Eq FragmentName
derive newtype instance ordFragmentName :: Ord FragmentName
instance showFragmentName :: Show FragmentName where
  show (FragmentName n) = "(FragmentName" <> Name.toString n <> ")"

toName :: FragmentName -> Name
toName (FragmentName n) = n

asFragmentName :: Name -> Maybe FragmentName
asFragmentName name@(Name.Name n)
  | n /= "on" = Just $ FragmentName name
  | otherwise = Nothing

type InlineFragment =
  { typeCond :: TypeCondition
  , directives :: Directives
  , selects :: SelectionSet
  }