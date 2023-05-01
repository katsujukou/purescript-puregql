module Test.GraphQL.Server.LexerSpec where

import Prelude

import Data.Either (Either(..), isLeft)
import Data.Foldable (for_)
import Data.Tuple (snd)
import Data.Tuple.Nested ((/\))
import GraphQL.Parsing.Lexer (blockStringValue, floatValue, intValue, name, punctuator, runLexer, stringValue)
import GraphQL.Parsing.Syntax (Token(..))
import Test.GraphQL.Parsing.Utils (shouldFailToTokenizeYielding)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "GraphQL.Server.Lexer" do
  describe "punctuator" do
    let
      cases =
        [ "!" /\ Right TokExclaim
        , "$" /\ Right TokDollar
        , "(" /\ Right TokLeftParens
        , ")" /\ Right TokRightParens
        , ":" /\ Right TokColon
        , "=" /\ Right TokEquals
        , "@" /\ Right TokAt
        , "[" /\ Right TokLeftSquare
        , "]" /\ Right TokRightSquare
        , "{" /\ Right TokLeftBrace
        , "|" /\ Right TokPipe
        , "}" /\ Right TokRightBrace
        , "..." /\ Right TokSpread
        ]

    it "should successfully tokenize punctuators" do
      for_ cases \(inp /\ expect) -> do
        snd (runLexer punctuator inp) `shouldEqual` expect

    it "should fail to tokenize invalid sequence of punctuator characters" do
      let rest /\ res = runLexer punctuator "..@{}"
      rest `shouldEqual` "..@{}"
      isLeft res `shouldEqual` true

  describe "name" do
    it "should successfully tokenize names" do
      let
        expects =
          [ "foo" /\ "" /\ Right (TokName "foo")
          , "foo1 bar baz" /\ " bar baz" /\ Right (TokName "foo1")
          , "_x" /\ "" /\ Right (TokName "_x")
          , "_1" /\ "" /\ Right (TokName "_1")
          ]
      for_ expects \(inp /\ expect) -> do
        runLexer name inp `shouldEqual` expect

    it "should fail to tokenize names beginning with digit" do
      let rest /\ res = runLexer name "1x"
      rest `shouldEqual` "1x"
      isLeft res `shouldEqual` true

  describe "intValue" do
    it "should successfully tokenize integer value" do
      let
        expects =
          [ "0" /\ "" /\ (Right $ TokInt 0)
          , "42a" /\ "a" /\ (Right $ TokInt 42)
          , "-0.1" /\ ".1" /\ (Right $ TokInt 0)
          , "-42" /\ "" /\ (Right $ TokInt (-42))
          ]
      for_ expects \(inp /\ expect) -> do
        runLexer intValue inp `shouldEqual` expect

  describe "floatValue" do
    it "should successfully tokenize float value" do
      let
        expects =
          [ "3.14x" /\ "x" /\ (Right $ TokFloat 3.14)
          , "-2.718" /\ "" /\ (Right $ TokFloat (-2.718))
          , "3e5e" /\ "e" /\ (Right $ TokFloat (3e5))
          , "-3e+5" /\ "" /\ (Right $ TokFloat (-3e5))
          , "5e-5" /\ "" /\ (Right $ TokFloat (5e-5))
          , "-12e-6" /\ "" /\ (Right $ TokFloat (-12e-6))
          , "4.5e-2" /\ "" /\ (Right $ TokFloat 4.5e-2)
          ]
      for_ expects \(inp /\ expect) -> do
        runLexer floatValue inp `shouldEqual` expect

  describe "stringValue" do
    it "should successfully tokenize string value" do
      let
        expects =
          [ "\"foo bar\" baz" /\ " baz" /\ (Right $ TokString "foo bar" "foo bar")
          -- containing escaped charancters
          , "\"abc\\ndef\\txyz\\b\"" /\ "" /\ (Right $ TokString "abc\\ndef\\txyz\\b" "abc\ndef\txyz\x0008")
          -- containing unicode literal
          , "\"Pok\\u00E9mon\"" /\ "" /\ (Right $ TokString "Pok\\u00E9mon" "Pok\x00E9mon")
          ]
      for_ expects \(inp /\ expect) -> do
        runLexer stringValue inp `shouldEqual` expect

    it "should fail to tokenize string containing line terminator" do
      runLexer stringValue "\"abc\ndef\""
        `shouldFailToTokenizeYielding` "\ndef\""
  describe "blockStringValue" do
    it "should successfully tokenize block string" do
      let
        expects =
          [ "\"\"\"\nabc\n  def\n  efg\n\"\"\"" /\ "" /\ Right (TokBlockString "abc\n  def\n  efg")
          -- escape characters (\n, \r, \t...) in block string should be treated as is.
          , "\"\"\"abc\\n\"def\"\\tghi\"\"\"" /\ "" /\ Right (TokBlockString "abc\\n\"def\"\\tghi")
          -- leading whitespaces of common length and line terminator in both ends should be omitted.
          , "\"\"\"\n    abc\n      def\n     ghi\"\"\"" /\ "" /\ Right (TokBlockString "abc\n  def\n ghi")
          ]
      for_ expects \(inp /\ expect) -> do
        runLexer blockStringValue inp `shouldEqual` expect