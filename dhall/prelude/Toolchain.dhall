--| Toolchain definitions for Buck2
--|
--| These generate toolchain BUCK files from Dhall.

let T = ./Types.dhall

-- ══════════════════════════════════════════════════════════════════════════════
-- C++ Toolchain (LLVM)
-- ══════════════════════════════════════════════════════════════════════════════

let CxxToolchain =
      { name : Text
      , c_extra_flags : List Text
      , cxx_extra_flags : List Text
      , link_flags : List Text
      , link_style : Text
      , vis : T.Vis
      }

let cxxToolchain
    : Text -> CxxToolchain
    = \(name : Text) ->
        { name
        , c_extra_flags = [] : List Text
        , cxx_extra_flags = [] : List Text
        , link_flags = [] : List Text
        , link_style = "static"
        , vis = T.Vis.Public
        }

-- ══════════════════════════════════════════════════════════════════════════════
-- Haskell Toolchain
-- ══════════════════════════════════════════════════════════════════════════════

let HaskellToolchain =
      { name : Text
      , compiler_flags : List Text
      , vis : T.Vis
      }

let haskellToolchain
    : Text -> HaskellToolchain
    = \(name : Text) ->
        { name
        , compiler_flags = [ "-Wall", "-Werror", "-XGHC2024" ]
        , vis = T.Vis.Public
        }

-- ══════════════════════════════════════════════════════════════════════════════
-- Execution Platform
-- ══════════════════════════════════════════════════════════════════════════════

let ExecutionPlatform =
      { name : Text
      , local_enabled : Bool
      , remote_enabled : Bool
      , vis : T.Vis
      }

let executionPlatform
    : Text -> ExecutionPlatform
    = \(name : Text) ->
        { name
        , local_enabled = True
        , remote_enabled = False
        , vis = T.Vis.Public
        }

-- ══════════════════════════════════════════════════════════════════════════════
-- Python Bootstrap (required by Buck2)
-- ══════════════════════════════════════════════════════════════════════════════

let PythonBootstrap =
      { name : Text
      , vis : T.Vis
      }

let pythonBootstrap
    : Text -> PythonBootstrap
    = \(name : Text) ->
        { name
        , vis = T.Vis.Public
        }

-- ══════════════════════════════════════════════════════════════════════════════
-- Genrule Toolchain (required by Buck2)
-- ══════════════════════════════════════════════════════════════════════════════

let GenruleToolchain =
      { name : Text
      , vis : T.Vis
      }

let genruleToolchain
    : Text -> GenruleToolchain
    = \(name : Text) ->
        { name
        , vis = T.Vis.Public
        }

in  { CxxToolchain
    , cxxToolchain
    , HaskellToolchain
    , haskellToolchain
    , ExecutionPlatform
    , executionPlatform
    , PythonBootstrap
    , pythonBootstrap
    , GenruleToolchain
    , genruleToolchain
    }
