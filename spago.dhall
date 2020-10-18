{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "record-optics-extra"
, dependencies = [ "profunctor-lenses", "psci-support", "spec" ]
, packages = ./packages.dhall
, license = "MIT"
, repository = "https://github.com/mcneissue/purescript-record-optics-extra"
}
