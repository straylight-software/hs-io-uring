--| Lean 4 Rules

let T = ./Types.dhall

let Binary =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , deps = [] : List T.Dep
        , vis = T.Vis.Public
        }

let Library =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , vis : T.Vis
      }

let library
    : Text -> List Text -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , deps = [] : List T.Dep
        , vis = T.Vis.Public
        }

in  { Binary, binary, Library, library }
