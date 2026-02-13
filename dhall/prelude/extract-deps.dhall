--| Extract flake refs from BUILD.dhall

let P = ./Prelude.dhall
let T = ./Types.dhall
let C = ./Cxx.dhall

let flakes = \(ds : List T.Dep) ->
    P.List.concatMap T.Dep Text
      (\(d : T.Dep) -> merge { Local = \(_ : Text) -> [] : List Text
                             , Flake = \(r : Text) -> [r] } d) ds

in  \(bs : List C.Binary) ->
      P.Text.concatSep "\n"
        (P.List.concatMap C.Binary Text (\(b : C.Binary) -> flakes b.deps) bs)
