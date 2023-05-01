module GraphQL.Parsing.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import GraphQL.Parsing.Source (SourcePhrase)

data LineTerminator
  = LF
  | CR
  | CRLF

data Token
  = TokExclaim
  | TokDollar
  | TokLeftParens
  | TokRightParens
  | TokSpread
  | TokColon
  | TokEquals
  | TokAt
  | TokLeftSquare
  | TokRightSquare
  | TokLeftBrace
  | TokRightBrace
  | TokPipe
  | TokName String
  | TokInt Int
  | TokFloat Number
  | TokString String String
  | TokBlockString String
  | TokEOF

derive instance eqToken :: Eq Token
derive instance genericToken :: Generic Token _

instance showToken :: Show Token where
  show = genericShow

type TokenPhrase = SourcePhrase Token
