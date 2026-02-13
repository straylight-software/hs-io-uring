{- Resource.dhall

   The coeffect algebra. Resources are what builds *require* from the environment.
   
   This is not effects (what builds do). This is coeffects (what builds need).
   
   Note: Dhall doesn't support recursive types directly. We use a flat list
   representation for combined resources instead of a recursive tree.
-}

let Resource
    : Type
    = < Pure                              -- needs nothing external
      | Network                           -- needs network access
      | Auth : Text                       -- needs credential (provider name)
      | Sandbox : Text                    -- needs isolation (sandbox type)
      | Filesystem : Text                 -- needs filesystem path
      >

-- A set of resources (representing combination via ⊗)
let Resources : Type = List Resource

let pure : Resources = [] : List Resource

let network : Resources = [ Resource.Network ]

let auth : Text → Resources = λ(provider : Text) → [ Resource.Auth provider ]

let sandbox : Text → Resources = λ(name : Text) → [ Resource.Sandbox name ]

let filesystem : Text → Resources = λ(path : Text) → [ Resource.Filesystem path ]

-- Combine two resource sets (the ⊗ operator)
let combine
    : Resources → Resources → Resources
    = λ(r : Resources) →
      λ(s : Resources) →
        r # s  -- list concatenation

-- ASCII alias for the tensor product
let tensor = combine

-- Check if resources are pure (empty)
let isPure : Resources → Bool = λ(r : Resources) →
    merge
      { None = True
      , Some = λ(_ : Resource) → False
      }
      (List/head Resource r)

-- Render a single resource to text
let renderOne : Resource → Text = λ(r : Resource) →
    merge
      { Pure = "pure"
      , Network = "network"
      , Auth = λ(x : Text) → "auth:${x}"
      , Sandbox = λ(x : Text) → "sandbox:${x}"
      , Filesystem = λ(x : Text) → "fs:${x}"
      }
      r

in  { Resource
    , Resources
    , pure
    , network
    , auth
    , sandbox
    , filesystem
    , combine
    , tensor
    , isPure
    , renderOne
    }
