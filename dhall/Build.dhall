{- Build.dhall

   Build target definition with coeffects.
-}

let Resource = ./Resource.dhall
let Toolchain = ./Toolchain.dhall
let Triple = ./Triple.dhall

-- Dependency reference
let Dep
    : Type
    = < -- Local target (same repo)
        Local : Text           -- ":foo" or "//pkg:foo"
        -- External (content-addressed)
      | External : { hash : Text, name : Text }
        -- Pkg-config
      | PkgConfig : Text       -- "openssl", "zlib"
        -- Nix flake reference (resolved at graph construction)
      | Flake : Text           -- "nixpkgs#openssl", ".#libfoo", "github:owner/repo#pkg"
      >

-- Source specification
let Src
    : Type
    = < -- Local files
        Files : List Text
        -- Content-addressed fetch
      | Fetch : { url : Text, hash : Text }
        -- Git
      | Git : { url : Text, rev : Text, hash : Text }
      >

-- Build target
let Target
    : Type
    = { name : Text
      , srcs : Src
      , deps : List Dep
      , toolchain : Toolchain.Toolchain
      , requires : Resource.Resources  -- coeffects (what this build needs)
      }

-- Convenience constructor
let target
    : Target → Target
    = λ(t : Target) → t

-- C/C++ library
let cxx-library
    : { name : Text
      , srcs : List Text
      , deps : List Dep
      , toolchain : Toolchain.Toolchain
      , requires : Resource.Resources
      } → Target
    = λ(cfg : { name : Text
              , srcs : List Text
              , deps : List Dep
              , toolchain : Toolchain.Toolchain
              , requires : Resource.Resources
              }) →
        { name = cfg.name
        , srcs = Src.Files cfg.srcs
        , deps = cfg.deps
        , toolchain = cfg.toolchain
        , requires = cfg.requires
        }

-- C/C++ binary
let cxx-binary
    : { name : Text
      , srcs : List Text
      , deps : List Dep
      , toolchain : Toolchain.Toolchain
      , requires : Resource.Resources
      } → Target
    = λ(cfg : { name : Text
              , srcs : List Text
              , deps : List Dep
              , toolchain : Toolchain.Toolchain
              , requires : Resource.Resources
              }) →
        { name = cfg.name
        , srcs = Src.Files cfg.srcs
        , deps = cfg.deps
        , toolchain = cfg.toolchain
        , requires = cfg.requires
        }

-- Fetch from URL (content-addressed)
let fetch
    : { url : Text, hash : Text } → Src
    = Src.Fetch

-- Git source
let git
    : { url : Text, rev : Text, hash : Text } → Src
    = Src.Git

-- Dependency constructors
let dep =
      { local = Dep.Local
      , pkgconfig = Dep.PkgConfig
      , external = λ(hash : Text) → λ(name : Text) → 
          Dep.External { hash, name }
      , flake = Dep.Flake
      , nixpkgs = λ(pkg : Text) → Dep.Flake "nixpkgs#${pkg}"
      , nixpkgsMusl = λ(pkg : Text) → Dep.Flake "nixpkgs#pkgsMusl.${pkg}"
      , nixpkgsStatic = λ(pkg : Text) → Dep.Flake "nixpkgs#pkgsStatic.${pkg}"
      }

in  { Dep
    , Src
    , Target
    , target
    , cxx-library
    , cxx-binary
    , fetch
    , git
    , dep
    , Resource = Resource
    , Toolchain = Toolchain
    , Triple = Triple
    }
