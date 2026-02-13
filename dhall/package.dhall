{- package.dhall

   Entry point for Armitage Dhall types.
   
   Import this to get all types:
     let Armitage = ./package.dhall
     
   Then use:
     Armitage.Triple.x86_64-linux-gnu
     Armitage.Resource.network
     Armitage.Build.cxx-binary { ... }
-}

let Triple = ./Triple.dhall
let Resource = ./Resource.dhall
let CFlags = ./CFlags.dhall
let LDFlags = ./LDFlags.dhall
let Toolchain = ./Toolchain.dhall
let Build = ./Build.dhall
let DischargeProof = ./DischargeProof.dhall

in  { Triple
    , Resource
    , CFlags
    , LDFlags
    , Toolchain
    , Build
    , DischargeProof
    }
