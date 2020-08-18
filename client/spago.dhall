{ name =
    "reactions"
, dependencies =
    [ "affjax"
    , "console"
    , "effect"
    , "elmish"
    , "elmish-html"
    , "foreign"
    , "psci-support"
    , "uuid"
    ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
}
