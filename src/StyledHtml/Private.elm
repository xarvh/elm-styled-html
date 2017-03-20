module StyledHtml.Private exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes


type alias StyleSnippet =
    String


type alias Rule =
    { selector : String
    , styleSnippets : List StyleSnippet
    }


type alias Class =
    { name : String
    , rules : String
    }


type Attribute msg
    = HtmlAttribute (Html.Attribute msg)
    | StyleAttribute (List Class)


type Html msg
    = Node String (List (Attribute msg)) (List (Html msg))
    | Text String


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute f a =
    case a of
        HtmlAttribute htmlAttr ->
            HtmlAttribute (Html.Attributes.map f htmlAttr)

        StyleAttribute classes ->
            StyleAttribute classes


render : Dict String Class -> Html msg -> ( Dict String Class, Html.Html msg )
render rulesBySelector0 styledHtmlNode =
    case styledHtmlNode of
        Text content ->
            ( rulesBySelector0, Html.text content )

        Node tagName styledAttributes styledChildren ->
            let
                -- Styled attributes contain both actual html attributes and style rules.
                -- Here we get the actual html attributes and style rules from the styled attributes.
                foldStyledAttribute styledAttribute ( rulesBySelector, htmlAttributes ) =
                    case styledAttribute of
                        HtmlAttribute htmlAttribute ->
                            ( rulesBySelector, htmlAttribute :: htmlAttributes )

                        StyleAttribute classes ->
                            let
                                newRules =
                                    List.foldl (\class d -> Dict.insert class.name class d) rulesBySelector classes

                                htmlAttribute =
                                    classes
                                        |> List.map .name
                                        |> String.join " "
                                        |> Html.Attributes.class
                            in
                                ( newRules, htmlAttribute :: htmlAttributes )

                ( rulesBySelector1, htmlAttributes ) =
                    List.foldr foldStyledAttribute ( rulesBySelector0, [] ) styledAttributes

                -- Styled html contains both actual virtual dom nodes and style rules
                foldChild styledChild ( rulesBySelector, htmlChildren ) =
                    let
                        ( newRules, htmlChild ) =
                            render rulesBySelector styledChild
                    in
                        ( newRules, htmlChild :: htmlChildren )

                -- TODO using rulesBySelectorX is a *terrible* idea
                ( rulesBySelector2, htmlChildren ) =
                    List.foldr foldChild ( rulesBySelector1, [] ) styledChildren

                html =
                    Html.node tagName htmlAttributes htmlChildren
            in
                ( rulesBySelector2, html )
