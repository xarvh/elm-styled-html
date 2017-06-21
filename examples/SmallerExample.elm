module Main exposing (..)

import AddStylesToDocumentHeader
import StyledHtml exposing (..)
import StyledHtml.Attributes exposing (ruleset)
import StyledHtml.Events exposing (onClick)
import StyledHtml.Css exposing (makeRuleset, selector)


redBackgroundRuleset =
    makeRuleset "redBackground"
        [ "background-color: #e00" ]
        []


greenBackgroundRuleset =
    makeRuleset "greenBackground"
        [ "background-color: #0e0" ]
        []


view ( showGreen, hasRuleset ) =
    div
        []
        [ div
            []
            [ if hasRuleset then
                text "The document header now defines both .redBackground and .greenBackground"
              else
                text "The document header defines .redBackground, but not .greenBackground"
            ]
        , button
            [ onClick (not showGreen) ]
            [ text "Click me to toggle the green div!" ]
        , div
            [ ruleset redBackgroundRuleset ]
            [ text "This div has .redBackground!" ]
        , if showGreen then
            div
                [ ruleset greenBackgroundRuleset ]
                [ text "This div has .greenBackground!" ]
          else
            text ""
        ]


main =
    program
        { init = ( ( False, False ), Cmd.none )
        , update = \msg ( showGreen, hasRuleset ) -> ( ( not showGreen, True ), Cmd.none )
        , view = view
        , subscriptions = always Sub.none
        , addStyles = AddStylesToDocumentHeader.addStylesToDocumentHeader
        }
