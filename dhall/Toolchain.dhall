{- Toolchain.dhall

   A toolchain is: compiler + host + target + flags
   
   Cross-compilation is just: host ≠ target
-}

let Triple = ./Triple.dhall
let CFlags = ./CFlags.dhall
let LDFlags = ./LDFlags.dhall

let Compiler
    : Type
    = < Clang : { version : Text }
      | NVClang : { version : Text }   -- LLVM git with CUDA C++23 patches
      | GCC : { version : Text }
      | NVCC : { version : Text }      -- NVIDIA CUDA compiler
      | Rustc : { version : Text }
      | GHC : { version : Text }
      | Lean : { version : Text }
      >

let Toolchain
    : Type
    = { compiler : Compiler
      , host : Triple.Triple
      , target : Triple.Triple
      , cflags : List CFlags.CFlag
      , cxxflags : List CFlags.CFlag
      , ldflags : List LDFlags.LDFlag
      , sysroot : Optional Text
      }

-- Note: Cross-compilation detection (host ≠ target) must happen in Nix,
-- as Dhall only supports == on Bool, not on Text or union types.
-- The build system compares Triple.render host vs Triple.render target.

-- Convenience constructors
let clang
    : { version : Text
      , host : Triple.Triple
      , target : Triple.Triple
      , cflags : List CFlags.CFlag
      , ldflags : List LDFlags.LDFlag
      } → Toolchain
    = λ(cfg : { version : Text
              , host : Triple.Triple
              , target : Triple.Triple
              , cflags : List CFlags.CFlag
              , ldflags : List LDFlags.LDFlag
              }) →
        { compiler = Compiler.Clang { version = cfg.version }
        , host = cfg.host
        , target = cfg.target
        , cflags = cfg.cflags
        , cxxflags = cfg.cflags  -- default: same as cflags
        , ldflags = cfg.ldflags
        , sysroot = None Text
        }

let gcc
    : { version : Text
      , host : Triple.Triple
      , target : Triple.Triple
      , cflags : List CFlags.CFlag
      , ldflags : List LDFlags.LDFlag
      } → Toolchain
    = λ(cfg : { version : Text
              , host : Triple.Triple
              , target : Triple.Triple
              , cflags : List CFlags.CFlag
              , ldflags : List LDFlags.LDFlag
              }) →
        { compiler = Compiler.GCC { version = cfg.version }
        , host = cfg.host
        , target = cfg.target
        , cflags = cfg.cflags
        , cxxflags = cfg.cflags
        , ldflags = cfg.ldflags
        , sysroot = None Text
        }

-- NV-Clang constructor (CUDA C++23)
let nv-clang
    : { version : Text
      , host : Triple.Triple
      , target : Triple.Triple
      , cflags : List CFlags.CFlag
      , ldflags : List LDFlags.LDFlag
      } → Toolchain
    = λ(cfg : { version : Text
              , host : Triple.Triple
              , target : Triple.Triple
              , cflags : List CFlags.CFlag
              , ldflags : List LDFlags.LDFlag
              }) →
        { compiler = Compiler.NVClang { version = cfg.version }
        , host = cfg.host
        , target = cfg.target
        , cflags = cfg.cflags
        , cxxflags = cfg.cflags
        , ldflags = cfg.ldflags
        , sysroot = None Text
        }

--------------------------------------------------------------------------------
-- Preset toolchains
--------------------------------------------------------------------------------

let presets =
      let native = Triple.x86_64-linux-gnu
      
      in  { -- Native dynamic (glibc)
            clang-18-glibc-dynamic = clang
              { version = "18"
              , host = native
              , target = native
              , cflags = [ CFlags.opt.O2, CFlags.warn.all, CFlags.warn.error ]
              , ldflags = [] : List LDFlags.LDFlag
              }
            
            -- Native static (musl)
          , clang-18-musl-static = clang
              { version = "18"
              , host = Triple.x86_64-linux-musl
              , target = Triple.x86_64-linux-musl
              , cflags = 
                  [ CFlags.opt.O2
                  , CFlags.warn.all
                  , CFlags.warn.error
                  , CFlags.static
                  ]
              , ldflags = [ LDFlags.static, LDFlags.gc-sections, LDFlags.strip ]
              }
            
            -- Cross to aarch64 (glibc)
          , clang-18-aarch64-cross = clang
              { version = "18"
              , host = native
              , target = Triple.aarch64-linux-gnu
              , cflags = [ CFlags.opt.O2, CFlags.warn.all ]
              , ldflags = [] : List LDFlags.LDFlag
              }
            
            -- WASM
          , clang-18-wasm = clang
              { version = "18"
              , host = native
              , target = Triple.wasm32-wasi
              , cflags = [ CFlags.opt.Oz ]
              , ldflags = [] : List LDFlags.LDFlag
              }

            ------------------------------------------------------------------------
            -- CUDA toolchains
            ------------------------------------------------------------------------
            
            -- H100 (Hopper) with x86_64 host
          , nv-clang-h100 = nv-clang
              { version = "git"
              , host = native
              , target = Triple.x86_64-h100
              , cflags = 
                  [ CFlags.opt.O3
                  , CFlags.std.cxx23
                  , CFlags.lto.thin
                  ]
              , ldflags = [ LDFlags.gc-sections ]
              }
            
            -- Grace Hopper (GH200)
          , nv-clang-grace-hopper = nv-clang
              { version = "git"
              , host = Triple.grace-hopper
              , target = Triple.grace-hopper
              , cflags =
                  [ CFlags.opt.O3
                  , CFlags.std.cxx23
                  , CFlags.lto.thin
                  ]
              , ldflags = [ LDFlags.gc-sections ]
              }
            
            -- Jetson Orin (embedded)
          , nv-clang-orin = nv-clang
              { version = "git"
              , host = native
              , target = Triple.jetson-orin
              , cflags =
                  [ CFlags.opt.O2
                  , CFlags.std.cxx20     -- embedded may lag on C++23
                  ]
              , ldflags = [] : List LDFlags.LDFlag
              }
            
            -- Blackwell (next-gen)
          , nv-clang-blackwell = nv-clang
              { version = "git"
              , host = native
              , target = Triple.dgx-blackwell
              , cflags =
                  [ CFlags.opt.O3
                  , CFlags.std.cxx23
                  , CFlags.lto.full
                  ]
              , ldflags = [ LDFlags.gc-sections ]
              }
          }

in  { -- Types
      Compiler
    , Toolchain
      -- Constructors
    , clang
    , gcc
    , nv-clang
      -- Presets
    , presets
    }
