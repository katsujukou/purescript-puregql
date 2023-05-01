{ name = "puregql"
, dependencies =
  [ "aff"
  , "arrays"
  , "bifunctors"
  , "console"
  , "control"
  , "debug"
  , "effect"
  , "either"
  , "enums"
  , "exceptions"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "maybe"
  , "numbers"
  , "ordered-collections"
  , "partial"
  , "prelude"
  , "st"
  , "strings"
  , "transformers"
  , "tuples"
  , "uncurried-transformers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
