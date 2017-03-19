module StyledHtml.Attributes exposing (..)

import StyledHtml.Css
import StyledHtml
    exposing
        ( Attribute(..)
        , Class
        , StyleSnippet
        , Rule
        )


class : Class -> Attribute msg
class class =
    StyleAttribute class


style : List StyleSnippet -> List (List Rule) -> Attribute msg
style styleSnippets compositeRules =
    class <| StyledHtml.Css.makeClass "_anon_" styleSnippets compositeRules