{- Example: hello world build target

   Demonstrates typed toolchains, flags, and coeffects.
   Pure build - needs nothing external.
-}

let Build = ../Build.dhall
let Resource = ../Resource.dhall
let Toolchain = ../Toolchain.dhall

in  Build.cxx-binary
      { name = "hello"
      , srcs = ["src/main.cpp"]
      , deps = [] : List Build.Dep
      , toolchain = Toolchain.presets.clang-18-musl-static
      , requires = Resource.pure  -- no external resources needed
      }
