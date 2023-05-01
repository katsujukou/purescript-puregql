module GraphQL.Parsing.Source where

type SourcePos =
  { ln :: Int
  , col :: Int
  }

type SourceRegion =
  { left :: SourcePos
  , right :: SourcePos
  }

type SourcePhrase a =
  { at :: SourceRegion
  , it :: a
  }

