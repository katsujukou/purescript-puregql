module Test.GraphQL.Parsing.Utils where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Either (Either, isLeft)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Exception (Error)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

shouldFailToTokenizeYielding :: forall m e a. Show e => Show a => Monad m => (String /\ Either e a) -> String -> MonadError Error m => m Unit
shouldFailToTokenizeYielding (inp /\ res) inp' = do
  res `shouldSatisfy` isLeft
  inp `shouldEqual` inp'
