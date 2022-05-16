{ name = "cps"
, dependencies =
  [ "console"
  , "control"
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
}
