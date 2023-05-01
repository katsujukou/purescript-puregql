module Test.GraphQL.Parsing.UtilsSpec where

import Prelude

import Data.String.Regex.Flags (unicode)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple.Nested ((/\))
import GraphQL.Parsing.Utils (spanRegex)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "GraphQL.Parsing.Utils" do
  describe "spanRegex" do
    it "should split matched part with given pattern" do
      spanRegex (unsafeRegex "^Pure" unicode) "PureScript"
        `shouldEqual` ("Pure" /\ "Script")

      spanRegex (unsafeRegex "^Pure" unicode) "TypeScript"
        `shouldEqual` ("" /\ "TypeScript")

      spanRegex (unsafeRegex "^\\d+[xyz]{3}" unicode) "13549xxyzabc"
        `shouldEqual` ("13549xxy" /\ "zabc")
