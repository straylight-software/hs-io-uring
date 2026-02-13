{- LDFlags.dhall

   Typed linker flags.
-}

let LDFlag
    : Type
    = < -- Linking mode
        Static
      | Shared
      | Pie
      | NoPie
      | Relocatable        -- -r
        
        -- Libraries
      | Lib : Text         -- -l<lib>
      | LibPath : Text     -- -L<path>
        
        -- Runtime
      | Rpath : Text
      | RpathLink : Text
        
        -- Symbols
      | Strip              -- -s
      | StripDebug         -- -S
      | ExportDynamic      -- --export-dynamic
      | AsNeeded           -- --as-needed
      | NoAsNeeded         -- --no-as-needed
        
        -- Garbage collection
      | GcSections         -- --gc-sections
      | NoGcSections
      | PrintGcSections    -- --print-gc-sections
        
        -- Output control
      | Soname : Text      -- -soname=<name>
      | VersionScript : Text
        
        -- LTO
      | LTOJobs : Natural  -- -flto-jobs=N
        
        -- Escape hatch
      | Raw : Text
      >

-- Render to string
let render
    : LDFlag → Text
    = λ(f : LDFlag) →
        merge
          { Static = "-static"
          , Shared = "-shared"
          , Pie = "-pie"
          , NoPie = "-no-pie"
          , Relocatable = "-r"
          , Lib = λ(l : Text) → "-l${l}"
          , LibPath = λ(p : Text) → "-L${p}"
          , Rpath = λ(p : Text) → "-Wl,-rpath,${p}"
          , RpathLink = λ(p : Text) → "-Wl,-rpath-link,${p}"
          , Strip = "-s"
          , StripDebug = "-S"
          , ExportDynamic = "-Wl,--export-dynamic"
          , AsNeeded = "-Wl,--as-needed"
          , NoAsNeeded = "-Wl,--no-as-needed"
          , GcSections = "-Wl,--gc-sections"
          , NoGcSections = "-Wl,--no-gc-sections"
          , PrintGcSections = "-Wl,--print-gc-sections"
          , Soname = λ(n : Text) → "-Wl,-soname,${n}"
          , VersionScript = λ(p : Text) → "-Wl,--version-script,${p}"
          , LTOJobs = λ(n : Natural) → "-flto-jobs=${Natural/show n}"
          , Raw = λ(r : Text) → r
          }
          f

-- Convenience
let static = LDFlag.Static
let shared = LDFlag.Shared
let pie = LDFlag.Pie
let strip = LDFlag.Strip
let strip-debug = LDFlag.StripDebug
let gc-sections = LDFlag.GcSections
let as-needed = LDFlag.AsNeeded
let export-dynamic = LDFlag.ExportDynamic

let lib
    : Text → LDFlag
    = LDFlag.Lib

let lib-path
    : Text → LDFlag
    = LDFlag.LibPath

let rpath
    : Text → LDFlag
    = LDFlag.Rpath

in  { LDFlag
    , render
    -- Convenience
    , static
    , shared
    , pie
    , strip
    , strip-debug
    , gc-sections
    , as-needed
    , export-dynamic
    , lib
    , lib-path
    , rpath
    }
