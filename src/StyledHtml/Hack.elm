module StyledHtml.Hack exposing (..)



{-| Important: the function signature is actually:

    toHtml : StyledHtml.Html msg -> Html.Html msg

The two types of `Html` are different, but the generated Elm docs will confuse the two types.

The function is a quick way to turn Styled Html into normal Html.

    normalHtml : Html.Html
    normalHtml =
      StyledHtml.toHtml someStyledHtml

The input html is wrapped inside a `div` together with a `style` tag:
```
<div>
  <style>
  ...generated CSS stylesheet goes here...
  </style>
  ..transformed someStyledHtml..
</div>
```
-}
toHtml : Html msg -> Html.Html msg
toHtml styledHtml =
    let
        ( style, html ) =
            renderStyleAndHtml styledHtml
    in
        Html.div
            []
            [ Html.node "style" [] [ Html.text style ]
            , html
            ]



{-| -}
beginnerProgram :
    { model : model
    , view : model -> Html msg
    , update : msg -> model -> model
    }
    -> Program Never model msg
beginnerProgram args =
    Html.beginnerProgram { args | view = toHtml << args.view }


{-| -}
program :
    { init : ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> Program Never model msg
program args =
    Html.program { args | view = toHtml << args.view }


{-| -}
programWithFlags :
    { init : flags -> ( model, Cmd msg )
    , update : msg -> model -> ( model, Cmd msg )
    , subscriptions : model -> Sub msg
    , view : model -> Html msg
    }
    -> Program flags model msg
programWithFlags args =
    Html.programWithFlags { args | view = toHtml << args.view }
