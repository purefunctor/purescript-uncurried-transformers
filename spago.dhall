{ name = "cps"
, dependencies =
  [ "console"
  , "control"
  , "effect"
  , "either"
  , "functions"
  , "prelude"
  , "tailrec"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
