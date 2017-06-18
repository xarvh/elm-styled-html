module Main exposing (..)

import AddStylesToDocumentHeader
import StyledHtml exposing (..)
import StyledHtml.Attributes exposing (class, style)
import StyledHtml.Events exposing (onClick)
import StyledHtml.Css exposing (makeClass, selector)


redBackgroundClass =
    makeClass "redBackground"
        [ "background-color: #e00" ]
        []


greenBackgroundClass =
    makeClass "greenBackground"
        [ "background-color: #0e0" ]
        []


view ( showGreen, hasClass ) =
    div
        []
        [ div
            []
            [ if hasClass then
                text "The document header now defines both .redBackground and .greenBackground"
              else
                text "The document header defines .redBackground, but not .greenBackground"
            ]
        , button
            [ onClick (not showGreen) ]
            [ text "Click me to toggle the green div!" ]
        , div
            [ class redBackgroundClass ]
            [ text "This div has .redBackground!" ]
        , if showGreen then
            div
                [ class greenBackgroundClass ]
                [ text "This div has .greenBackground!" ]
          else
            text ""
        ]


main =
    program
        { init = ( ( False, False ), Cmd.none )
        , update = \msg ( showGreen, hasClass ) -> ( ( not showGreen, True ), Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , addStyles = AddStylesToDocumentHeader.addStylesToDocumentHeader
        }
