module StyledHtml exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import String.Extra


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


extendSelector left right =
    -- TODO: what if `rule` starts with an html tag name?
    --> Break the rule after the first continuous set of a-z and insert the class name there
    left ++ right


{-|
  Takes a styleSnippets list and rules and puts all of them under the provided selector
  TODO: this function should be exposed as something like "andSelector ..."
-}
pushScope : String -> List StyleSnippet -> List (List Rule) -> List Rule
pushScope selector styleSnippets compositeRules =
    let
        unscopedRules =
            { selector = "", styleSnippets = styleSnippets } :: List.concat compositeRules

        scopedRules =
            unscopedRules
                |> List.map (\rule -> { rule | selector = extendSelector selector rule.selector })
    in
        scopedRules


andClass : Class -> List StyleSnippet -> List (List Rule) -> List Rule
andClass class styleSnippets compositeRules =
    pushScope ("." ++ class.name) styleSnippets compositeRules


hover : List StyleSnippet -> List (List Rule) -> List Rule
hover styleSnippets compositeRules =
    pushScope ":hover" styleSnippets compositeRules


{-| TODO: use an actual hash function
-}
calculateHash =
    String.length >> toString


{-|
  Ideally this function is called only once at start up, so we want to
  put here most of the crunching and processing.
-}
makeClass : String -> List StyleSnippet -> List (List Rule) -> Class
makeClass rawClassName styleSnippets compositeRules =
    let
        -- TODO: ensure it does not make sense as a CSS selector
        classNamePlaceholder =
            "@@classname@@"

        rulesWithPlaceholder =
            pushScope ("." ++ classNamePlaceholder) styleSnippets compositeRules

        -- TODO: This should be a configured option
        hash =
            -- TODO: properly convert the rules to a string?
            calculateHash (toString rulesWithPlaceholder)

        -- TODO: the function to turn raw class name and hash into actual class name should be a configured option?
        finalClassName =
            rawClassName ++ "_" ++ hash

        snippetToString snippet =
            "  " ++ snippet ++ ";\n"

        snippetsToString snippets =
            snippets
                |> List.map snippetToString
                |> String.join ""

        ruleToString rule =
            String.Extra.replace classNamePlaceholder finalClassName rule.selector ++ " {\n" ++ snippetsToString rule.styleSnippets ++ "}\n"

        actualRules =
            rulesWithPlaceholder
                |> List.map ruleToString
                |> String.join "\n"
    in
        { name = finalClassName
        , rules = actualRules
        }



--


type Attribute msg
    = HtmlAttribute (Html.Attribute msg)
    | StyleAttribute Class


type Node msg
    = Node String (List (Attribute msg)) (List (Node msg))
    | Text String


text : String -> Node msg
text content =
    Text content


node : String -> List (Attribute msg) -> List (Node msg) -> Node msg
node tag attributes children =
    Node tag attributes children


useClass : Class -> Attribute msg
useClass class =
    StyleAttribute class


anonymous : List StyleSnippet -> List (List Rule) -> Attribute msg
anonymous styleSnippets compositeRules =
    useClass <| makeClass "_anon_" styleSnippets compositeRules



-- rendering


type alias RulesDict =
    Dict String Class


render : RulesDict -> Node msg -> ( RulesDict, Html msg )
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


getContentAndStyle : Node msg -> ( String, Html msg )
getContentAndStyle styledHtml =
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
