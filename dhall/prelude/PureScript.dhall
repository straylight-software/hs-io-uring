--| PureScript Rules

let T = ./Types.dhall

-- | PureScript web application (Halogen, etc.)
let App =
      { name : Text
      , srcs : List Text
      , spago_dhall : Text
      , packages_dhall : Optional Text
      , main : Text
      , index_html : Optional Text
      , style_css : Optional Text
      , vis : T.Vis
      }

let app
    : Text -> List Text -> Text -> App
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(spago_dhall : Text) ->
        { name, srcs, spago_dhall
        , packages_dhall = Some "packages.dhall"
        , main = "Main"
        , index_html = Some "index.html"
        , style_css = Some "style.css"
        , vis = T.Vis.Public
        }

-- | PureScript Node.js binary
let Binary =
      { name : Text
      , srcs : List Text
      , spago_dhall : Text
      , packages_dhall : Optional Text
      , main : Text
      , vis : T.Vis
      }

let binary
    : Text -> List Text -> Text -> Binary
    = \(name : Text) ->
      \(srcs : List Text) ->
      \(spago_dhall : Text) ->
        { name, srcs, spago_dhall
        , packages_dhall = Some "packages.dhall"
        , main = "Main"
        , vis = T.Vis.Public
        }

-- | PureScript library
let Library =
      { name : Text
      , srcs : List Text
      , spago_yaml : Optional Text
      , vis : T.Vis
      }

let library
    : Text -> List Text -> Library
    = \(name : Text) ->
      \(srcs : List Text) ->
        { name, srcs
        , spago_yaml = None Text
        , vis = T.Vis.Public
        }

in  { App, app, Binary, binary, Library, library }
