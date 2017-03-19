module Main exposing (..)

import Html
import StyledHtml


-- attributes can be repeated in StyledHtml.Attributes, so that they can be used together other attrs?
--> Yes, it makes StyledHtml into a drop-in replacement for html!


div =
    StyledHtml.node "div"


text =
    StyledHtml.makeClass "text"
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
        , StyledHtml.andClass text
            [ "background-color: yellow"
            ]
            []
        ]


styles =
    { text = text
    , button = button
    }


view =
    div
        [ StyledHtml.inline [ "background-color: red;" ] []
        ]
        [ div
            [ StyledHtml.useClass styles.text
            , StyledHtml.inline [ "background-color: blue;" ] []
            ]
            [ text "I am some text" ]
        , div
            [ StyledHtml.useClass styles.button ]
            [ text "I look like a button" ]
        ]


main =
    let
        ( content, style ) =
            StyledHtml.getContentAndStyle view
    in
        Html.div
            []
            [ Html.node "style"
                []
                [ Html.text style ]
            , content
            ]
