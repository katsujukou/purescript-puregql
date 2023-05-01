module GraphQL.Parsing.Lexer where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array (mapWithIndex, span)
import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Array.ST as STArray
import Data.Bifunctor (rmap)
import Data.Either (Either(..))
import Data.Enum (toEnum)
import Data.Foldable (fold)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Int (hexadecimal)
import Data.Int as Int
import Data.Maybe (Maybe(..), isNothing)
import Data.Number as Number
import Data.String (CodePoint, codePointFromChar, joinWith, toCodePointArray)
import Data.String as String
import Data.String.CodeUnits as SCU
import Data.String.Regex as Regex
import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested (type (/\), (/\))
import GraphQL.Parsing.Errors (ParseError(..))
import GraphQL.Parsing.Syntax (LineTerminator(..), Token(..))
import GraphQL.Parsing.Utils (is, isBeginningOfName, isName, isStringCharacter, isWhitespace, punctuatorChars, spanRegex, splitChunksWith, takeWhile)
import Partial.Unsafe (unsafePartial)

type LexResult a = String /\ (Either ParseError a)

newtype Lexer a = Lexer
  (String -> LexResult a)

instance functorLexer :: Functor Lexer where
  map f (Lexer k) = Lexer \inp -> rmap (map f) (k inp)

instance applyLexer :: Apply Lexer where
  apply (Lexer k1) (Lexer k2) = Lexer \inp ->
    let
      inp' /\ ef = k1 inp
    in
      case ef of
        Left err -> inp' /\ Left err
        Right f ->
          let
            next /\ ea = k2 inp'
          in
            next /\ map f ea

instance applicativeLexer :: Applicative Lexer where
  pure a = Lexer $ \inp -> inp /\ Right a

instance bindLexer :: Bind Lexer where
  bind (Lexer k1) f = Lexer $ \inp ->
    let
      inp' /\ ea = k1 inp
    in
      case ea of
        Left err -> inp' /\ Left err
        Right a ->
          let
            Lexer k2 = f a
          in
            k2 inp'

instance monadLexer :: Monad Lexer

instance altLexer :: Alt Lexer where
  alt (Lexer k1) (Lexer k2) = Lexer $ \inp ->
    let
      inp' /\ ea = k1 inp
    in
      case ea of
        Right a -> inp' /\ Right a
        Left _ -> k2 inp'

many :: forall a. Lexer a -> Lexer (Array a)
many (Lexer k) = Lexer \str -> ST.run do
  valuesRef <- STArray.new
  strRef <- STRef.new str
  contRef <- STRef.new true
  resRef <- STRef.new (str /\ Right [])
  ST.while (STRef.read contRef) do
    str' <- STRef.read strRef
    case k str' of
      str'' /\ Left error
        | SCU.length str' == SCU.length str'' -> do
            values <- STArray.unsafeFreeze valuesRef
            _ <- STRef.write (str'' /\ Right values) resRef
            _ <- STRef.write false contRef
            pure unit
        | otherwise -> do
            _ <- STRef.write (str'' /\ Left error) resRef
            _ <- STRef.write false contRef
            pure unit
      str'' /\ Right a -> do
        _ <- STArray.push a valuesRef
        _ <- STRef.write str'' strRef
        pure unit
  STRef.read resRef

try :: forall a. Lexer a -> Lexer a
try (Lexer k) = Lexer \inp ->
  let
    inp' /\ ea = k inp
  in
    case ea of
      Right a -> inp' /\ Right a
      Left e -> inp /\ Left e

runLexer :: forall a. Lexer a -> String -> LexResult a
runLexer (Lexer k) inp = k inp

type Tokenizer = Lexer Token

-- lex :: String -> LexResult Token
-- lex = runLexer lexer

-- lexer :: Tokenizer
-- lexer = Lexer \inp ->
--   case String.uncons inp of
--     Nothing -> "" /\ Right TokEOF
--     Just { head: cp, tail: rest }
--       | isPunctuator cp -> tokPunctuator rest cp
--       | isBeginningOfName cp -> tokName rest cp
--       | isBeginningOfNumericValue cp -> tokNumeric cp inp
--       | otherwise -> rest /\ Left (Unexpected cp)

punctuator :: Tokenizer
punctuator = Lexer \inp -> case String.uncons inp of
  Nothing -> "" /\ Left UnexpectedEOF
  Just { head: cp, tail: rest }
    | cp `is` '!' -> rest /\ Right TokExclaim
    | cp `is` '$' -> rest /\ Right TokDollar
    | cp `is` '(' -> rest /\ Right TokLeftParens
    | cp `is` ')' -> rest /\ Right TokRightParens
    | cp `is` ':' -> rest /\ Right TokColon
    | cp `is` '=' -> rest /\ Right TokEquals
    | cp `is` '@' -> rest /\ Right TokAt
    | cp `is` '[' -> rest /\ Right TokLeftSquare
    | cp `is` ']' -> rest /\ Right TokRightSquare
    | cp `is` '{' -> rest /\ Right TokLeftBrace
    | cp `is` '|' -> rest /\ Right TokPipe
    | cp `is` '}' -> rest /\ Right TokRightBrace
    | cp `is` '.'
    , String.take 2 rest == ".." -> (String.drop 2 rest) /\ Right TokSpread
    | otherwise -> inp /\ Left (Expected punctuatorChars (String.singleton cp))

name :: Tokenizer
name = Lexer \inp ->
  case String.uncons inp of
    Nothing -> "" /\ Left UnexpectedEOF
    Just { head, tail }
      | isBeginningOfName head
      , rest /\ next <- takeWhile isName tail -> next /\ Right (TokName $ String.singleton head <> rest)
      | otherwise -> inp /\ Left (Expected "name character" (mkUnexpected inp))

intValue :: Tokenizer
intValue = Lexer \inp -> case String.uncons inp of
  Nothing -> "" /\ Left UnexpectedEOF
  Just { head }
    | sat /\ rest <- spanRegex (unsafeRegex "^-?(0|[1-9][0-9]*)" unicode) inp
    , Just v <- Int.fromString sat -> rest /\ Right (TokInt v)
    | otherwise -> inp /\ Left (Expected "^-?(0|[1-9][0-9]*)" (String.singleton head))

floatValue :: Tokenizer
floatValue = unsafePartial do
  TokInt intPart <- intValue
  floatPart <-
    ( try do
        flac <- flactionalPart
        exp <- exponentPart
        pure $ flac <> exp
    )
      <|> flactionalPart
      <|> exponentPart
  unsafePartial $ case Number.fromString (show intPart <> floatPart) of
    Just v -> pure (TokFloat v)

  where
  flactionalPart = Lexer \inp ->
    case String.uncons inp of
      Nothing -> "" /\ Left UnexpectedEOF
      Just { head }
        | frac /\ rest <- spanRegex (unsafeRegex "^\\.[0-9]+" unicode) inp
        , frac /= "" -> rest /\ Right frac
        | otherwise -> inp /\ Left (Expected "^\\.[0-9]+" (String.singleton head))

  exponentPart = regex (Expected "float value exponent part") "[eE][+-]?(0|[1-9])[0-9]*"

stringValue :: Tokenizer
stringValue = do
  part <- (quote *> many stringCharater <* quote)
  let { raw, string } = fold part
  pure $ TokString raw string
  where
  stringCharater = stringSourceChar
    <|> stringEscapedChar
    <|> stringEscapedUnicode

  stringSourceChar = do
    ch <- satisfy isStringCharacter
    let raw = String.singleton ch
    pure { raw, string: raw }

  stringEscapedChar = try $ backslash *>
    Lexer \inp -> case String.uncons inp of
      Nothing -> "" /\ Left UnexpectedEOF
      Just { head: cp, tail: rest }
        | cp `is` '"' -> rest /\ Right { raw: "\\\"", string: "\"" }
        | cp `is` '\\' -> rest /\ Right { raw: "\\\\", string: "\\" }
        | cp `is` '/' -> rest /\ Right { raw: "\\/", string: "/" }
        | cp `is` 'b' -> rest /\ Right { raw: "\\b", string: "\x0008" }
        | cp `is` 'n' -> rest /\ Right { raw: "\\n", string: "\n" }
        | cp `is` 'r' -> rest /\ Right { raw: "\\r", string: "\r" }
        | cp `is` 't' -> rest /\ Right { raw: "\\t", string: "\t" }
        | otherwise -> inp /\ Left (InvalidCharEscape $ String.singleton cp)

  stringEscapedUnicode = try $
    (backslash >>= const (char 'u')) *> do
      hex <- regex (Expected "escaped unicode") """[0-9a-fA-F]{4}"""
      unsafePartial $ case Int.fromStringAs hexadecimal hex of
        Just charCode
          | charCode >= 0x0000
          , charCode <= 0xFFFF
          , Just string <- String.singleton <$> toEnum charCode -> pure { raw: "\\u" <> hex, string }

blockStringValue :: Tokenizer
blockStringValue = do
  part <- tripleQuote *> many blockStringCharacter <* tripleQuote
  let lines = map fold $ splitChunksWith (_ == "\n") part
  pure $ TokBlockString (mkBlockString lines)
  where
  blockStringCharacter = Lexer \inp -> case String.uncons inp of
    Nothing -> "" /\ Left UnexpectedEOF
    Just { head: cp, tail: rest }
      | cp `is` '"' ->
          case String.take 2 rest of
            "\"\"" -> inp /\ Left InvalidTripleQuoteInBlockString
            _ -> rest /\ Right "\""
      | otherwise -> rest /\ Right (String.singleton cp)

  -- Implementation is based on `Block String semantics` in the GraphQL specicifation.
  -- See: https://spec.graphql.org/June2018/#StringValue
  mkBlockString lines =
    ST.run
      do
        commonIndentRef <- STRef.new Nothing
        linesRef <- STRef.new lines
        forWithIndex_ lines \i line -> do
          unless (i == 0) do
            let
              len = String.length line
              leadingWhitespace /\ _ = takeWhile isWhitespace line
              indent = String.length leadingWhitespace
            when (indent < len) do
              commonIndent <- STRef.read commonIndentRef
              when (isNothing commonIndent || Just indent < commonIndent) do
                _ <- STRef.write (Just indent) commonIndentRef
                pure unit
        STRef.read commonIndentRef >>= case _ of
          Nothing -> pure unit
          Just commonIndent -> do
            void $
              STRef.modify (mapWithIndex (\i line -> if i == 0 then line else String.drop commonIndent line)) linesRef

        void $ do
          _ <- flip STRef.modify linesRef \lines' -> case Array.uncons lines' of
            Just { head: h, tail }
              | { rest: [] } <- span isWhitespace $ toCodePointArray h -> tail
            _ -> lines'

          flip STRef.modify linesRef \lines' -> case Array.unsnoc lines' of
            Just { init, last: l }
              | { rest: [] } <- span isWhitespace $ toCodePointArray l -> init
            _ -> lines'

        (joinWith "\n") <$> STRef.read linesRef

lineTerminator :: Lexer LineTerminator
lineTerminator = Lexer \inp -> case String.uncons inp of
  Nothing -> "" /\ Left UnexpectedEOF
  Just { head: cp, tail: rest }
    | cp `is` '\n' -> rest /\ Right LF
    | cp `is` '\r' -> case String.uncons rest of
        Just { head: cp', tail: rest' }
          | cp' `is` '\n' -> rest' /\ Right CRLF
        _ -> rest /\ Right CR
    | otherwise -> inp /\ Left (Expected "line feed" (String.singleton cp))

mkUnexpected :: String -> String
mkUnexpected str = do
  let start = String.take 6 str
  let len = String.length start
  if len == 0 then
    "<end of input>"
  else if len < 6 then
    start
  else
    start <> "..."

regex :: (String -> ParseError) -> String -> Lexer String
regex mkErr regexString = Lexer \inp ->
  case String.uncons inp of
    Nothing -> "" /\ Left UnexpectedEOF
    _ -> case Regex.match matchRegex inp of
      Just group
        | Just matched <- NonEmptyArray.head group -> (String.drop (String.length matched) inp) /\ Right matched
      _ -> inp /\ Left (mkErr inp)
  where
  matchRegex = unsafeRegex ("^(?:" <> regexString <> ")") unicode

quote :: Lexer String
quote = codePoint (codePointFromChar '"')

tripleQuote :: Lexer String
tripleQuote = do
  _ <- quote
  _ <- quote
  _ <- quote
  pure "\"\"\""

backslash :: Lexer String
backslash = codePoint (codePointFromChar '\\')

char :: Char -> Lexer CodePoint
char ch = satisfy (_ `is` ch)

codePoint :: CodePoint -> Lexer String
codePoint cp = Lexer \inp ->
  let
    expect = String.singleton cp
  in
    case String.uncons inp of
      Nothing -> "" /\ Left (Expected (String.singleton cp) ("<eof>"))
      Just { head, tail }
        | head == cp -> tail /\ Right (String.singleton cp)
        | otherwise -> inp /\ Left (Expected expect (String.singleton head))

satisfy :: (CodePoint -> Boolean) -> Lexer CodePoint
satisfy p = Lexer \inp -> case String.uncons inp of
  Nothing -> "" /\ Left UnexpectedEOF
  Just { head: ch, tail: rest }
    | p ch -> rest /\ Right ch
    | otherwise -> inp /\ Left (Unexpected ch)