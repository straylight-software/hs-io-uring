--| NVIDIA CUDA Rules (using clang, not nvcc)

let T = ./Types.dhall

let Binary =
      { name : Text
      , srcs : List Text
      , deps : List T.Dep
      , archs : List Text
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , deps = [] : List T.Dep
        , archs = [] : List Text  -- uses default from buckconfig
        , vis = T.Vis.Public
        }

let Library =
      { name : Text
      , srcs : List Text
      , exported_headers : List Text
      , deps : List T.Dep
      , archs : List Text
      , vis : T.Vis
      }

let library
    : Text -> List Text -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , exported_headers = [] : List Text
        , deps = [] : List T.Dep
        , archs = [] : List Text
        , vis = T.Vis.Public
        }

in  { Binary, binary, Library, library }
