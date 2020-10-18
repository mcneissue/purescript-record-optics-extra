let mkPackage =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.0-20190626/src/mkPackage.dhall sha256:0b197efa1d397ace6eb46b243ff2d73a3da5638d8d0ac8473e8e4a8fc528cf57

let upstream =
      https://raw.githubusercontent.com/purescript/package-sets/psc-0.13.8-20201007/src/packages.dhall sha256:35633f6f591b94d216392c9e0500207bb1fec42dd355f4fecdfd186956567b6b

let overrides = {=}

let additions = {=}

in  upstream // overrides // additions
