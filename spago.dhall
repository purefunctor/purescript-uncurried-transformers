{ name = "cps"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "either"
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
