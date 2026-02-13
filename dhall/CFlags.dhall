{- CFlags.dhall

   Typed C/C++ compiler flags.
   
   No more string soup. The compiler catches typos.
-}

let CStd
    : Type
    = < c89 | c99 | c11 | c17 | c23 >

let CxxStd
    : Type
    = < cxx11 | cxx14 | cxx17 | cxx20 | cxx23 >

let OptLevel
    : Type
    = < O0 | O1 | O2 | O3 | Os | Oz | Og >

let LTOMode
    : Type
    = < off | thin | full >

let Sanitizer
    : Type
    = < address | memory | thread | undefined | leak >

let DebugLevel
    : Type
    = < g0 | g1 | g2 | g3 >

let CFlag
    : Type
    = < -- Optimization
        Opt : OptLevel
      | LTO : LTOMode
        
        -- Standards
      | StdC : CStd
      | StdCxx : CxxStd
        
        -- Warnings
      | Wall
      | Wextra
      | Werror
      | Wpedantic
      | Wno : Text           -- -Wno-<warning>
        
        -- Preprocessor
      | Define : { name : Text, value : Optional Text }
      | Undef : Text
      | Include : Text       -- -I<path>
      | System : Text        -- -isystem <path>
        
        -- Code generation
      | PIC
      | PIE
      | Static
      | Shared
        
        -- Architecture
      | March : Text         -- -march=<arch>
      | Mtune : Text         -- -mtune=<cpu>
      | Native               -- -march=native
        
        -- Debug
      | Debug : DebugLevel
        
        -- Sanitizers
      | Sanitize : Sanitizer
        
        -- Misc
      | FunctionSections     -- -ffunction-sections
      | DataSections         -- -fdata-sections
      | NoExceptions         -- -fno-exceptions
      | NoRTTI               -- -fno-rtti
      | Pthread              -- -pthread
        
        -- Escape hatch (logged, warned)
      | Raw : Text
      >

-- Render a flag to string
let render
    : CFlag → Text
    = λ(f : CFlag) →
        merge
          { Opt = λ(o : OptLevel) →
              merge
                { O0 = "-O0", O1 = "-O1", O2 = "-O2", O3 = "-O3"
                , Os = "-Os", Oz = "-Oz", Og = "-Og"
                }
                o
          , LTO = λ(l : LTOMode) →
              merge
                { off = "", thin = "-flto=thin", full = "-flto"
                }
                l
          , StdC = λ(s : CStd) →
              merge
                { c89 = "-std=c89", c99 = "-std=c99", c11 = "-std=c11"
                , c17 = "-std=c17", c23 = "-std=c23"
                }
                s
          , StdCxx = λ(s : CxxStd) →
              merge
                { cxx11 = "-std=c++11", cxx14 = "-std=c++14"
                , cxx17 = "-std=c++17", cxx20 = "-std=c++20"
                , cxx23 = "-std=c++23"
                }
                s
          , Wall = "-Wall"
          , Wextra = "-Wextra"
          , Werror = "-Werror"
          , Wpedantic = "-Wpedantic"
          , Wno = λ(w : Text) → "-Wno-${w}"
          , Define = λ(d : { name : Text, value : Optional Text }) →
              merge
                { None = "-D${d.name}"
                , Some = λ(v : Text) → "-D${d.name}=${v}"
                }
                d.value
          , Undef = λ(u : Text) → "-U${u}"
          , Include = λ(p : Text) → "-I${p}"
          , System = λ(p : Text) → "-isystem ${p}"
          , PIC = "-fPIC"
          , PIE = "-fPIE"
          , Static = "-static"
          , Shared = "-shared"
          , March = λ(a : Text) → "-march=${a}"
          , Mtune = λ(c : Text) → "-mtune=${c}"
          , Native = "-march=native"
          , Debug = λ(d : DebugLevel) →
              merge
                { g0 = "-g0", g1 = "-g1", g2 = "-g2", g3 = "-g3"
                }
                d
          , Sanitize = λ(s : Sanitizer) →
              merge
                { address = "-fsanitize=address"
                , memory = "-fsanitize=memory"
                , thread = "-fsanitize=thread"
                , undefined = "-fsanitize=undefined"
                , leak = "-fsanitize=leak"
                }
                s
          , FunctionSections = "-ffunction-sections"
          , DataSections = "-fdata-sections"
          , NoExceptions = "-fno-exceptions"
          , NoRTTI = "-fno-rtti"
          , Pthread = "-pthread"
          , Raw = λ(r : Text) → r
          }
          f

-- Convenience constructors
let opt =
      { O0 = CFlag.Opt OptLevel.O0
      , O1 = CFlag.Opt OptLevel.O1
      , O2 = CFlag.Opt OptLevel.O2
      , O3 = CFlag.Opt OptLevel.O3
      , Os = CFlag.Opt OptLevel.Os
      , Oz = CFlag.Opt OptLevel.Oz
      }

let std =
      { c11 = CFlag.StdC CStd.c11
      , c17 = CFlag.StdC CStd.c17
      , c23 = CFlag.StdC CStd.c23
      , cxx17 = CFlag.StdCxx CxxStd.cxx17
      , cxx20 = CFlag.StdCxx CxxStd.cxx20
      , cxx23 = CFlag.StdCxx CxxStd.cxx23
      }

let warn =
      { all = CFlag.Wall
      , extra = CFlag.Wextra
      , error = CFlag.Werror
      , pedantic = CFlag.Wpedantic
      }

let lto =
      { off = CFlag.LTO LTOMode.off
      , thin = CFlag.LTO LTOMode.thin
      , full = CFlag.LTO LTOMode.full
      }

let debug =
      { none = CFlag.Debug DebugLevel.g0
      , minimal = CFlag.Debug DebugLevel.g1
      , default = CFlag.Debug DebugLevel.g2
      , full = CFlag.Debug DebugLevel.g3
      }

let sanitize =
      { address = CFlag.Sanitize Sanitizer.address
      , memory = CFlag.Sanitize Sanitizer.memory
      , thread = CFlag.Sanitize Sanitizer.thread
      , undefined = CFlag.Sanitize Sanitizer.undefined
      }

let define
    : Text → Optional Text → CFlag
    = λ(name : Text) → λ(value : Optional Text) → CFlag.Define { name, value }

let include
    : Text → CFlag
    = CFlag.Include

in  { CStd
    , CxxStd
    , OptLevel
    , LTOMode
    , Sanitizer
    , DebugLevel
    , CFlag
    , render
    -- Convenience
    , opt
    , std
    , warn
    , lto
    , debug
    , sanitize
    , define
    , include
    , native = CFlag.Native
    , pic = CFlag.PIC
    , pie = CFlag.PIE
    , static = CFlag.Static
    , pthread = CFlag.Pthread
    , no-exceptions = CFlag.NoExceptions
    , no-rtti = CFlag.NoRTTI
    , function-sections = CFlag.FunctionSections
    , data-sections = CFlag.DataSections
    }
