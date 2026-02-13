--| Minimal Prelude (no network)

let concatSep
    : Text -> List Text -> Text
    = \(sep : Text) ->
      \(xs : List Text) ->
        let r = List/fold Text xs { fst : Bool, acc : Text }
              (\(x : Text) -> \(s : { fst : Bool, acc : Text }) ->
                if s.fst then { fst = False, acc = x }
                else { fst = False, acc = x ++ sep ++ s.acc })
              { fst = True, acc = "" }
        in r.acc

let map
    : forall (a : Type) -> forall (b : Type) -> (a -> b) -> List a -> List b
    = \(a : Type) -> \(b : Type) -> \(f : a -> b) -> \(xs : List a) ->
        List/build b (\(l : Type) -> \(c : b -> l -> l) ->
          List/fold a xs l (\(x : a) -> c (f x)))

let concatMap
    : forall (a : Type) -> forall (b : Type) -> (a -> List b) -> List a -> List b
    = \(a : Type) -> \(b : Type) -> \(f : a -> List b) -> \(xs : List a) ->
        List/build b (\(l : Type) -> \(c : b -> l -> l) ->
          List/fold a xs l (\(x : a) -> List/fold b (f x) l c))

let null
    : forall (a : Type) -> List a -> Bool
    = \(a : Type) -> \(xs : List a) ->
        Natural/isZero (List/length a xs)

in  { Text = { concatSep }, List = { map, concatMap, null } }
