--| Core Types

let Dep = < Local : Text | Flake : Text >

let CxxStd = < Cxx11 | Cxx14 | Cxx17 | Cxx20 | Cxx23 >

let Vis = < Public | Private >

in  { Dep
    , CxxStd
    , Vis
    , local = Dep.Local
    , flake = Dep.Flake
    , nix = \(p : Text) -> Dep.Flake "nixpkgs#${p}"
    }
