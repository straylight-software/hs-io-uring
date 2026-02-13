{- Example: build with nixpkgs dependency

   Demonstrates flake refs in deps - DICE resolves these during analysis.
-}

let Build = ../Build.dhall
let Resource = ../Resource.dhall
let Toolchain = ../Toolchain.dhall
let Triple = ../Triple.dhall
let CFlags = ../CFlags.dhall
let LDFlags = ../LDFlags.dhall

in Build.cxx-binary
  { name = "fmt-example"
  , srcs = ["src/armitage/dhall/examples/src/main.cpp"]
  , deps = 
      [ Build.dep.flake "nixpkgs#fmt"           -- fmt library from nixpkgs
      , Build.dep.nixpkgs "nlohmann_json"       -- convenience: expands to nixpkgs#nlohmann_json
      ]
  , toolchain = Toolchain.presets.clang-18-glibc-dynamic
  , requires = Resource.pure
  }
