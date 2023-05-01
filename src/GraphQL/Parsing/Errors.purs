module GraphQL.Parsing.Errors where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String (CodePoint)
import GraphQL.Parsing.Source (SourcePhrase)

data ParseError
  = UnexpectedEOF
  | InvalidCharEscape String
  | InvalidLineFeedInString
  | InvalidDoubleQuoteInString
  | InvalidTripleQuoteInBlockString
  | UnicodeLiteralCharOutOfRange String
  | Expected String String
  | Unexpected CodePoint

derive instance eqParseError :: Eq ParseError
derive instance genericParseError :: Generic ParseError _

instance showParseError :: Show ParseError where
  show = genericShow

type ErrorPhrase = SourcePhrase ParseError