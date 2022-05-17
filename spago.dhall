{ name = "uncurried-transformers"
, dependencies =
  [ "control"
  , "effect"
  , "either"
  , "functions"
  , "identity"
  , "prelude"
  , "safe-coerce"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
, license = "BSD-3-Clause"
, repository = "https://github.com/PureFunctor/purescript-uncurried-transformers.git"
}
