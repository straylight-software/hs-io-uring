--| C/C++ Rules

let T = ./Types.dhall

let Binary =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , std : T.CxxStd
      , cflags : List Text
      , ldflags : List Text
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> List T.Dep -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(deps : List T.Dep) ->
        { name, srcs, deps
        , std = T.CxxStd.Cxx17
        , cflags = [] : List Text
        , ldflags = [] : List Text
        , vis = T.Vis.Public
        }

let Library =
      { name : Text
      , srcs : List Text
      , hdrs : List Text
      , deps : List T.Dep
      , std : T.CxxStd
      , cflags : List Text
      , vis : T.Vis
      }

let library
    : Text -> List Text -> List T.Dep -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(deps : List T.Dep) ->
        { name, srcs, deps
        , hdrs = [] : List Text
        , std = T.CxxStd.Cxx17
        , cflags = [] : List Text
        , vis = T.Vis.Public
        }

in  { Binary, binary, Library, library }
