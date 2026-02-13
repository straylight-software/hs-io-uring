{- Example: llama inference with gated model and GPU

   Demonstrates coeffects: network, auth, sandbox.
   Uses H100 target with explicit GPU SM version.
-}

let Build = ../Build.dhall
let Resource = ../Resource.dhall
let Toolchain = ../Toolchain.dhall
let Triple = ../Triple.dhall
let CFlags = ../CFlags.dhall
let LDFlags = ../LDFlags.dhall

-- Use H100 preset (already has correct cpu=sapphirerapids, gpu=sm_90)
let h100-toolchain = Toolchain.presets.nv-clang-h100

-- Or define custom with explicit cpu/gpu targeting:
let custom-h100-toolchain = Toolchain.nv-clang
  { version = "git"
  , host = Triple.x86_64-linux-gnu
  , target = Triple.x86_64-h100  -- Includes gpu=sm_90
  , cflags = 
      [ CFlags.opt.O3
      , CFlags.std.cxx23
      , CFlags.warn.all
      , CFlags.define "GGML_CUDA" (Some "1")
      , CFlags.lto.thin
      ]
  , ldflags = [ LDFlags.gc-sections ]
  }

let llama-inference = Build.cxx-binary
  { name = "llama-inference"
  , srcs = 
      [ "src/main.cpp"
      , "src/model.cpp"
      , "src/inference.cpp"
      ]
  , deps = 
      [ Build.dep.local ":llama-cpp"
      , Build.dep.pkgconfig "cuda"
      ]
  , toolchain = custom-h100-toolchain
  
  -- The coeffects: what this build requires
  -- network: fetch model weights
  -- auth "huggingface": gated model access
  -- sandbox "gpu": GPU isolation
  , requires = 
      Resource.combine
        (Resource.combine Resource.network (Resource.auth "huggingface"))
        (Resource.sandbox "gpu")
  }

in  llama-inference
