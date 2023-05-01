module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.GraphQL.Parsing.UtilsSpec as GraphQL.Parsing.UtilsSpec
import Test.GraphQL.Server.LexerSpec as GraphQL.Parsing.LexerSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  GraphQL.Parsing.LexerSpec.spec
  GraphQL.Parsing.UtilsSpec.spec