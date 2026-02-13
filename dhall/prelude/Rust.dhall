--| Rust Rules

let T = ./Types.dhall

let Edition = < E2015 | E2018 | E2021 | E2024 >

let Binary =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , edition : Edition
      , features : List Text
      , rustflags : List Text
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> List T.Dep -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(deps : List T.Dep) ->
        { name, srcs, deps
        , edition = Edition.E2021
        , features = [] : List Text
        , rustflags = [] : List Text
        , vis = T.Vis.Public
        }

let Library =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , edition : Edition
      , crate_name : Optional Text
      , features : List Text
      , proc_macro : Bool
      , vis : T.Vis
      }

let library
    : Text -> List Text -> List T.Dep -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(deps : List T.Dep) ->
        { name, srcs, deps
        , edition = Edition.E2021
        , crate_name = None Text
        , features = [] : List Text
        , proc_macro = False
        , vis = T.Vis.Public
        }

in  { Edition, Binary, binary, Library, library }
