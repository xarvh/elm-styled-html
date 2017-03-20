module Main exposing (..)

import Html
import StyledHtml exposing (div, text)
import StyledHtml.Attributes as A
import StyledHtml.Events as E
import StyledHtml.Css as Css


classHeader =
    Css.makeClass "header"
        [ "font-size: 20px"
        ]
        []


classButton =
    Css.makeClass "button"
        [ "padding: 10px"
        , "border: 1px solid blue"
        ]
        [ Css.hover
            [ "background-color: purple"
            ]
            []
        , Css.andClass classHeader
            [ "background-color: yellow"
            ]
            []
        ]


view model =
    div
        -- A.style is going to re-calculate the hash every time, it might be slower than a named class
        [ A.style [ "background-color: red;" ] []
        ]
        [ div
            [ E.onClick ""
            , A.class classHeader
            , A.style
                [ "background-color: blue" ]
                [ Css.hover
                  [ "text-transform: uppercase"
                  ]
                  []
                ]
            ]
            [ text "I am some text"
            , text (toString model)
            ]
        , div
            [ A.class classButton ]
            [ text "I look like a button" ]
        ]


main =
    StyledHtml.beginnerProgram
      { model = 0
      , update = \msg model -> model + 1
      , view = view
      }
