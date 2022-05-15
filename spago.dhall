{ name = "cps"
, dependencies =
  [ "console"
  , "effect"
  , "functions"
  , "newtype"
  , "prelude"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
