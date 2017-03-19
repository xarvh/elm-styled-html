module Main exposing (..)

import Html
import StyledHtml exposing (div, text)
import StyledHtml.Attributes as A
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


view =
    div
        -- A.style is going to re-calculate the hash every time, it might be slower than a named class
        [ A.style [ "background-color: red;" ] []
        ]
        [ div
            [ A.class classHeader
            , A.style [ "background-color: blue;" ] []
            ]
            [ text "I am some text" ]
        , div
            [ A.class classButton ]
            [ text "I look like a button" ]
        ]


main =
    StyledHtml.toHtml view
