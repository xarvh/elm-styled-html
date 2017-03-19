module Main exposing (..)

import Html
import StyledHtml


-- attributes can be repeated in StyledHtml.Attributes, so that they can be used together other attrs?
--> Yes, it makes StyledHtml into a drop-in replacement for html!


div =
    StyledHtml.node "div"


header =
    StyledHtml.makeClass "header"
        [ "font-size: 20px"
        ]
        []


button =
    StyledHtml.makeClass "button"
        [ "padding: 10px"
        , "border: 1px solid blue"
        ]
        [ StyledHtml.hover
            [ "background-color: purple"
            ]
            []
        , StyledHtml.andClass header
            [ "background-color: yellow"
            ]
            []
        ]


styles =
    { header = header
    , button = button
    }


view =
    div
        -- StyledHtml.anonymous is going to re-calculate the hash every time, it might be slower than a named class
        [ StyledHtml.anonymous [ "background-color: red;" ] []
        ]
        [ div
            [ StyledHtml.useClass styles.header
            , StyledHtml.anonymous [ "background-color: blue;" ] []
            ]
            [ StyledHtml.text "I am some text" ]
        , div
            [ StyledHtml.useClass styles.button ]
            [ StyledHtml.text "I look like a button" ]
        ]


main =
    let
        ( style, html ) =
            StyledHtml.getContentAndStyle view

        q = Debug.log style ""
    in
        Html.div
            []
            [ Html.node "style" [] [ Html.text style ]
            , html
            ]
