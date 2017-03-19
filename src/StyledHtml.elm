module StyledHtml exposing (..)

import Dict exposing (Dict)
import Html
import Html.Attributes
import String.Extra


-- Primitives


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
    | StyleAttribute Class


type Html msg
    = Node String (List (Attribute msg)) (List (Html msg))
    | Text String



-- elements


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute f a =
    case a of
        HtmlAttribute htmlAttr ->
            HtmlAttribute (Html.Attributes.map f htmlAttr)

        StyleAttribute class ->
            StyleAttribute class


map : (a -> b) -> Html a -> Html b
map f htmlA =
    case htmlA of
        Text content ->
            Text content

        Node tag attributes children ->
            Node tag (List.map (mapAttribute f) attributes) (List.map (map f) children)


text : String -> Html msg
text content =
    Text content


node : String -> List (Attribute msg) -> List (Html msg) -> Html msg
node tag attributes children =
    Node tag attributes children


div =
    node "div"



-- programs


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



-- beginnerProgram args =
--     Html.beginnerProgram { args | view = \model -> styledToHtml <| args.view model }
-- program args =
--
-- programWithFlags args =
-- rendering


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

                        StyleAttribute class ->
                            let
                                newRules =
                                    Dict.insert class.name class rulesBySelector

                                htmlAttribute =
                                    Html.Attributes.class class.name
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


renderStyleAndHtml : Html msg -> ( String, Html.Html msg )
renderStyleAndHtml styledHtml =
    let
        ( rulesBySelector, html ) =
            render Dict.empty styledHtml

        style =
            rulesBySelector
                |> Dict.values
                |> List.map .rules
                |> String.join "\n\n"
    in
        ( style, html )
