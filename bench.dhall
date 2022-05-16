let conf = ./spago.dhall

in      conf
    //  { sources = conf.sources # [ "bench/**/*.purs" ]
        , dependencies =
              conf.dependencies
            # [ "arrays", "benchotron" ]
        }
