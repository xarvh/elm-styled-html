module Main exposing (..)

import StyledHtml exposing (..)
import StyledHtml.Attributes exposing (class, style)
import StyledHtml.Events exposing (onClick)
import StyledHtml.Css exposing (makeClass, selector)


redBackgroundClass =
    makeClass "redBackground"
        [ "background-color: #e00" ]
        []


view model =
    div
        [ class redBackgroundClass ]
        [ span
            [ onClick 5
            , style
                [ "background-color: blue"
                , "font-size: 30px"
                ]
                [ selector ":hover"
                    [ "background-color: cyan" ]
                    []
                ]
            ]
            [ text (toString model) ]
        ]


main =
    beginnerProgram
        { model = 0
        , update = \msg model -> model + msg
        , view = view
        }
