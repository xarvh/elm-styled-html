module StyledHtml.Private exposing (..)

import Dict exposing (Dict)
import Html as VanillaHtml
import Html.Attributes as VanillaHtmlAttributes
import Set exposing (Set)
import VirtualDom


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
    = VirtualDomProperty (VirtualDom.Property msg)
    | StyleAttribute (List Class)


type Html msg
    = StyledHtmlNode String (List (Attribute msg)) (List (Html msg))
    | VirtualDomNode (VirtualDom.Node msg)
    | Text String


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute f a =
    case a of
        VirtualDomProperty nodeProperty ->
            VirtualDomProperty (VirtualDom.mapProperty f nodeProperty)

        StyleAttribute classes ->
            StyleAttribute classes


render : Dict String Class -> Html msg -> ( Dict String Class, VirtualDom.Node msg )
render rulesBySelector0 styledHtmlNode =
    case styledHtmlNode of
        Text content ->
            ( rulesBySelector0, VirtualDom.text content )

        VirtualDomNode node ->
            ( rulesBySelector0, node )

        StyledHtmlNode tagName styledAttributes styledChildren ->
            let
                -- Styled attributes contain both actual virtual dom attributes and style rules.
                -- Here we get the actual virtual dom attributes and style rules from the styled attributes.
                foldStyledAttribute styledAttribute ( rulesBySelector, nodeAttributes ) =
                    case styledAttribute of
                        VirtualDomProperty property ->
                            ( rulesBySelector, property :: nodeAttributes )

                        StyleAttribute classes ->
                            let
                                newRules =
                                    List.foldl (\class d -> Dict.insert class.name class d) rulesBySelector classes

                                property =
                                    classes
                                        |> List.map .name
                                        |> String.join " "
                                        |> VanillaHtmlAttributes.class
                            in
                                ( newRules, property :: nodeAttributes )

                ( rulesBySelector1, nodeAttributes ) =
                    List.foldr foldStyledAttribute ( rulesBySelector0, [] ) styledAttributes

                -- Styled html contains both actual virtual dom nodes and style rules
                foldChild styledChild ( rulesBySelector, nodeChildren ) =
                    let
                        ( newRules, nodeChild ) =
                            render rulesBySelector styledChild
                    in
                        ( newRules, nodeChild :: nodeChildren )

                -- TODO rulesBySelectorX is a *terrible* name
                ( rulesBySelector2, nodeChildren ) =
                    List.foldr foldChild ( rulesBySelector1, [] ) styledChildren

                node =
                    VirtualDom.node tagName nodeAttributes nodeChildren
            in
                ( rulesBySelector2, node )



-- program


type alias ProgramModel userModel msg =
    { viewAsVanillaHtml : VanillaHtml.Html msg
    , addedCssSelectors : Set String
    , userModel : userModel
    }


makeStyles : Set String -> Dict String Class -> ( Set String, List String )
makeStyles selectorsAlreadyAdded rulesBySelectors =
    let
        selectorsAndRulesToAdd =
            rulesBySelectors
                |> Dict.toList
                |> List.filter (\( selector, rules ) -> not <| Set.member selector selectorsAlreadyAdded)

        stylesToAdd =
            selectorsAndRulesToAdd
                |> List.map (\( selector, class ) -> class.rules)

        selectorsToAdd =
            selectorsAndRulesToAdd
                |> List.map Tuple.first
                |> Set.fromList
    in
        ( Set.union selectorsToAdd selectorsAlreadyAdded, stylesToAdd )


wrappingModelAndCmd :
    (List String -> Cmd msg)
    -> (userModel -> Html msg)
    -> Set String
    -> ( userModel, Cmd msg )
    -> ( ProgramModel userModel msg, Cmd msg )
wrappingModelAndCmd addStyles view oldAddedCssSelectors ( userModel, userCmd ) =
    let
        ( style, viewAsVanillaHtml ) =
            render Dict.empty (view userModel)

        ( addedCssSelectors, stylesToAdd ) =
            makeStyles oldAddedCssSelectors style

        wrappingModel =
            { viewAsVanillaHtml = viewAsVanillaHtml
            , addedCssSelectors = addedCssSelectors
            , userModel = userModel
            }

        addStylesCmd =
            if stylesToAdd == [] then
                Cmd.none
            else
                addStyles stylesToAdd

        cmd =
            Cmd.batch [ addStylesCmd, userCmd ]
    in
        ( wrappingModel, cmd )
