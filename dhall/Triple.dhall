{- Triple.dhall

   Target triples: arch-vendor-os-abi + cpu/gpu microarchitecture
   
   Standard nomenclature from LLVM/GCC/Rust.
   Microarchitecture types from nix/prelude/types/Target.dhall.
-}

let Arch
    : Type
    = < x86_64
      | aarch64
      | riscv64
      | wasm32
      | armv7
      >

let Vendor
    : Type
    = < unknown
      | pc
      | apple
      | nvidia
      >

let OS
    : Type
    = < linux
      | darwin
      | windows
      | wasi
      | none          -- bare metal
      >

let ABI
    : Type
    = < gnu           -- glibc
      | musl          -- musl libc (static-friendly)
      | eabi          -- embedded
      | eabihf        -- embedded hard float
      | msvc          -- Microsoft
      | none          -- no libc
      >

--------------------------------------------------------------------------------
-- CPU Microarchitecture
--
-- These matter for -march/-mtune. Native builds should use `native`.
-- Cross builds must specify the exact target CPU.
--------------------------------------------------------------------------------

let Cpu
    : Type
    = < generic       -- safe baseline
      | native        -- detect at compile time (-march=native)
      -- x86_64
      | x86_64_v2     -- SSE4.2, POPCNT (Nehalem+)
      | x86_64_v3     -- AVX2, BMI2 (Haswell+)
      | x86_64_v4     -- AVX-512 (Skylake-X+)
      | znver3        -- AMD Zen 3
      | znver4        -- AMD Zen 4 (AVX-512)
      | znver5        -- AMD Zen 5
      | sapphirerapids -- Intel Sapphire Rapids
      | alderlake     -- Intel Alder Lake (hybrid)
      -- aarch64 datacenter (SBSA)
      | neoverse_v2   -- Grace (GH200, GB200, DGX Spark)
      | neoverse_n2   -- Altra, Ampere
      -- aarch64 embedded (Jetson)
      | cortex_a78ae  -- Orin (AGX/NX/Nano), Thor
      | cortex_a78c   -- Orin variants
      -- aarch64 consumer
      | apple_m1
      | apple_m2
      | apple_m3
      | apple_m4
      >

--------------------------------------------------------------------------------
-- GPU Microarchitecture (SM version)
--
-- For CUDA compilation. Used with -arch=sm_XX.
-- Only NVIDIA GPUs have SM versions; others use `none`.
--------------------------------------------------------------------------------

let Gpu
    : Type
    = < none
      -- Ampere
      | sm_80         -- A100
      | sm_86         -- RTX 30xx, A series
      -- Ada Lovelace
      | sm_89         -- RTX 40xx, L40
      -- Hopper
      | sm_90         -- H100 (PCIe)
      | sm_90a        -- H100 SXM (async features)
      -- Orin (embedded)
      | sm_87         -- Jetson Orin
      -- Blackwell
      | sm_100        -- B100, B200
      | sm_100a       -- B200 (full features)
      | sm_120        -- RTX 50xx
      >

let Triple
    : Type
    = { arch : Arch
      , vendor : Vendor
      , os : OS
      , abi : ABI
      , cpu : Cpu         -- CPU microarchitecture
      , gpu : Gpu         -- GPU SM version (none for non-NVIDIA)
      }

-- Render arch to string
let renderArch : Arch → Text = λ(a : Arch) →
    merge
      { x86_64 = "x86_64"
      , aarch64 = "aarch64"
      , riscv64 = "riscv64"
      , wasm32 = "wasm32"
      , armv7 = "armv7"
      }
      a

-- Render vendor to string
let renderVendor : Vendor → Text = λ(v : Vendor) →
    merge
      { unknown = "unknown"
      , pc = "pc"
      , apple = "apple"
      , nvidia = "nvidia"
      }
      v

-- Render OS to string
let renderOS : OS → Text = λ(o : OS) →
    merge
      { linux = "linux"
      , darwin = "darwin"
      , windows = "windows"
      , wasi = "wasi"
      , none = "none"
      }
      o

-- Render ABI to string (returns Optional for none case)
let renderABI : ABI → Optional Text = λ(a : ABI) →
    merge
      { gnu = Some "gnu"
      , musl = Some "musl"
      , eabi = Some "eabi"
      , eabihf = Some "eabihf"
      , msvc = Some "msvc"
      , none = None Text
      }
      a

-- Check if ABI is none
let abiIsNone : ABI → Bool = λ(a : ABI) →
    merge
      { gnu = False
      , musl = False
      , eabi = False
      , eabihf = False
      , msvc = False
      , none = True
      }
      a

-- Render CPU to string (for -march/-mtune)
let renderCpu : Cpu → Text = λ(c : Cpu) →
    merge
      { generic = "generic"
      , native = "native"
      , x86_64_v2 = "x86-64-v2"
      , x86_64_v3 = "x86-64-v3"
      , x86_64_v4 = "x86-64-v4"
      , znver3 = "znver3"
      , znver4 = "znver4"
      , znver5 = "znver5"
      , sapphirerapids = "sapphirerapids"
      , alderlake = "alderlake"
      , neoverse_v2 = "neoverse-v2"
      , neoverse_n2 = "neoverse-n2"
      , cortex_a78ae = "cortex-a78ae"
      , cortex_a78c = "cortex-a78c"
      , apple_m1 = "apple-m1"
      , apple_m2 = "apple-m2"
      , apple_m3 = "apple-m3"
      , apple_m4 = "apple-m4"
      }
      c

-- Render GPU to CUDA arch string
let renderGpu : Gpu → Optional Text = λ(g : Gpu) →
    merge
      { none = None Text
      , sm_80 = Some "sm_80"
      , sm_86 = Some "sm_86"
      , sm_87 = Some "sm_87"
      , sm_89 = Some "sm_89"
      , sm_90 = Some "sm_90"
      , sm_90a = Some "sm_90a"
      , sm_100 = Some "sm_100"
      , sm_100a = Some "sm_100a"
      , sm_120 = Some "sm_120"
      }
      g

-- Check if GPU is none
let gpuIsNone : Gpu → Bool = λ(g : Gpu) →
    merge
      { none = True
      , sm_80 = False
      , sm_86 = False
      , sm_87 = False
      , sm_89 = False
      , sm_90 = False
      , sm_90a = False
      , sm_100 = False
      , sm_100a = False
      , sm_120 = False
      }
      g

-- Render to canonical LLVM triple string (without cpu/gpu, those go to flags)
let render
    : Triple → Text
    = λ(t : Triple) →
        let archStr = renderArch t.arch
        let vendorStr = renderVendor t.vendor
        let osStr = renderOS t.os
        in  if abiIsNone t.abi
            then "${archStr}-${vendorStr}-${osStr}"
            else merge
              { Some = λ(abiStr : Text) → "${archStr}-${vendorStr}-${osStr}-${abiStr}"
              , None = "${archStr}-${vendorStr}-${osStr}"
              }
              (renderABI t.abi)

--------------------------------------------------------------------------------
-- Common triples
--------------------------------------------------------------------------------

-- x86_64 Linux (glibc)
let x86_64-linux-gnu
    : Triple
    = { arch = Arch.x86_64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- x86_64 Linux (musl, static-friendly)
let x86_64-linux-musl
    : Triple
    = { arch = Arch.x86_64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.musl
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- aarch64 Linux (glibc)
let aarch64-linux-gnu
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- aarch64 Linux (musl)
let aarch64-linux-musl
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.musl
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- macOS Apple Silicon
let aarch64-apple-darwin
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.apple
      , os = OS.darwin
      , abi = ABI.none
      , cpu = Cpu.apple_m1  -- base Apple Silicon
      , gpu = Gpu.none
      }

-- macOS Intel
let x86_64-apple-darwin
    : Triple
    = { arch = Arch.x86_64
      , vendor = Vendor.apple
      , os = OS.darwin
      , abi = ABI.none
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- WebAssembly
let wasm32-wasi
    : Triple
    = { arch = Arch.wasm32
      , vendor = Vendor.unknown
      , os = OS.wasi
      , abi = ABI.none
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

-- RISC-V 64-bit
let riscv64-linux-gnu
    : Triple
    = { arch = Arch.riscv64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.generic
      , gpu = Gpu.none
      }

--------------------------------------------------------------------------------
-- NVIDIA targets (specific CPU+GPU combinations)
--------------------------------------------------------------------------------

-- Grace Hopper (GH200) - datacenter
let grace-hopper
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.nvidia
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.neoverse_v2
      , gpu = Gpu.sm_90a      -- H100 SXM
      }

-- Jetson Orin (embedded)
let jetson-orin
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.nvidia
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.cortex_a78ae
      , gpu = Gpu.sm_87
      }

-- DGX Blackwell (next-gen datacenter)
let dgx-blackwell
    : Triple
    = { arch = Arch.aarch64
      , vendor = Vendor.nvidia
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.neoverse_v2
      , gpu = Gpu.sm_100a     -- B200 full features
      }

-- x86_64 with H100 (traditional datacenter)
let x86_64-h100
    : Triple
    = { arch = Arch.x86_64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.sapphirerapids
      , gpu = Gpu.sm_90
      }

-- x86_64 with RTX 4090 (consumer/prosumer)
let x86_64-rtx4090
    : Triple
    = { arch = Arch.x86_64
      , vendor = Vendor.unknown
      , os = OS.linux
      , abi = ABI.gnu
      , cpu = Cpu.znver4      -- Zen 4 is common pairing
      , gpu = Gpu.sm_89
      }

-- Note: Dhall doesn't support == on union types, so is-cross would require
-- comparing rendered strings. Cross detection happens in the build system
-- layer (Nix) not the configuration layer (Dhall).

in  { -- Types
      Arch
    , Vendor
    , OS
    , ABI
    , Cpu
    , Gpu
    , Triple
      -- Rendering
    , render
    , renderArch
    , renderVendor
    , renderOS
    , renderABI
    , renderCpu
    , renderGpu
    , abiIsNone
    , gpuIsNone
      -- Common triples (generic)
    , x86_64-linux-gnu
    , x86_64-linux-musl
    , aarch64-linux-gnu
    , aarch64-linux-musl
    , aarch64-apple-darwin
    , x86_64-apple-darwin
    , wasm32-wasi
    , riscv64-linux-gnu
      -- NVIDIA targets
    , grace-hopper
    , jetson-orin
    , dgx-blackwell
    , x86_64-h100
    , x86_64-rtx4090
    }
