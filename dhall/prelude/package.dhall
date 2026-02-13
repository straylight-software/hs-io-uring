--| Aleph Prelude

let T = ./Types.dhall
let C = ./Cxx.dhall
let R = ./Rust.dhall
let H = ./Haskell.dhall
let L = ./Lean.dhall
let N = ./Nv.dhall
let PS = ./PureScript.dhall
let TC = ./Toolchain.dhall

in  { -- Types
      Dep = T.Dep
    , CxxStd = T.CxxStd
    , RustEdition = R.Edition
    , Vis = T.Vis
    -- Dep constructors
    , local = T.local
    , flake = T.flake
    , nix = T.nix
    -- C++ rules
    , CxxBinary = C.Binary
    , cxxBinary = C.binary
    , CxxLibrary = C.Library
    , cxxLibrary = C.library
    -- Rust rules
    , RustBinary = R.Binary
    , rustBinary = R.binary
    , RustLibrary = R.Library
    , rustLibrary = R.library
    -- Haskell rules
    , HaskellBinary = H.Binary
    , haskellBinary = H.binary
    , HaskellLibrary = H.Library
    , haskellLibrary = H.library
    , HaskellFFIBinary = H.FFIBinary
    , haskellFFIBinary = H.ffiBinary
    -- Lean rules
    , LeanBinary = L.Binary
    , leanBinary = L.binary
    , LeanLibrary = L.Library
    , leanLibrary = L.library
    -- NVIDIA rules
    , NvBinary = N.Binary
    , nvBinary = N.binary
    , NvLibrary = N.Library
    , nvLibrary = N.library
    -- PureScript rules
    , PureScriptApp = PS.App
    , purescriptApp = PS.app
    , PureScriptBinary = PS.Binary
    , purescriptBinary = PS.binary
    , PureScriptLibrary = PS.Library
    , purescriptLibrary = PS.library
    -- Toolchains
    , CxxToolchain = TC.CxxToolchain
    , cxxToolchain = TC.cxxToolchain
    , HaskellToolchain = TC.HaskellToolchain
    , haskellToolchain = TC.haskellToolchain
    , ExecutionPlatform = TC.ExecutionPlatform
    , executionPlatform = TC.executionPlatform
    , PythonBootstrap = TC.PythonBootstrap
    , pythonBootstrap = TC.pythonBootstrap
    , GenruleToolchain = TC.GenruleToolchain
    , genruleToolchain = TC.genruleToolchain
    -- Backward compat (short names for common case)
    , Binary = C.Binary
    , binary = C.binary
    , Library = C.Library
    , library = C.library
    }
