--| Haskell Rules

let T = ./Types.dhall

let Binary =
      { name : Text
      , srcs : List Text
      , main : Text
      , packages : List Text
      , language_extensions : List Text
      , ghc_options : List Text
      , deps : List T.Dep
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , main = "Main"
        , packages = [ "base" ] : List Text
        , language_extensions = [] : List Text
        , ghc_options = [ "-O2", "-Wall" ] : List Text
        , deps = [] : List T.Dep
        , vis = T.Vis.Public
        }

let Library =
      { name : Text
      , srcs : List Text
      , packages : List Text
      , language_extensions : List Text
      , ghc_options : List Text
      , deps : List T.Dep
      , vis : T.Vis
      }

let library
    : Text -> List Text -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , packages = [ "base" ] : List Text
        , language_extensions = [] : List Text
        , ghc_options = [ "-O2", "-Wall" ] : List Text
        , deps = [] : List T.Dep
        , vis = T.Vis.Public
        }

let FFIBinary =
      { name : Text
      , hs_srcs : List Text
      , cxx_srcs : List Text
      , cxx_headers : List Text
      , packages : List Text
      , language_extensions : List Text
      , ghc_options : List Text
      , extra_libs : List Text
      , extra_lib_dirs : List Text
      , include_dirs : List Text
      , linker_flags : List Text
      , vis : T.Vis
      }

let ffiBinary
    : Text -> List Text -> List Text -> FFIBinary
    = \(name : Text) ->
      \(hs_srcs : List Text) ->
      \(cxx_srcs : List Text) ->
        { name, hs_srcs, cxx_srcs
        , cxx_headers = [] : List Text
        , packages = [ "base" ] : List Text
        , language_extensions = [] : List Text
        , ghc_options = [ "-O2" ] : List Text
        , extra_libs = [] : List Text
        , extra_lib_dirs = [] : List Text
        , include_dirs = [] : List Text
        , linker_flags = [] : List Text
        , vis = T.Vis.Public
        }

in  { Binary, binary, Library, library, FFIBinary, ffiBinary }
