module StyledHtml.Private exposing (..)

import Dict exposing (Dict)
import Html as VanillaHtml
import Html.Attributes as VanillaHtmlAttributes
import Set exposing (Set)
import VirtualDom


type alias Declaration =
    String


type alias Rule =
    { selector : String
    , declarations : List Declaration
    }


type alias Ruleset =
    { className : String
    , rules : String
    }


type Attribute msg
    = VirtualDomProperty (VirtualDom.Property msg)
    | StyleAttribute (List Ruleset)


type Html msg
    = StyledHtmlNode String (List (Attribute msg)) (List (Html msg))
    | VirtualDomNode (VirtualDom.Node msg)
    | Text String


mapAttribute : (a -> b) -> Attribute a -> Attribute b
mapAttribute f a =
    case a of
        VirtualDomProperty nodeProperty ->
            VirtualDomProperty (VirtualDom.mapProperty f nodeProperty)

        StyleAttribute rulesets ->
            StyleAttribute rulesets


render : Dict String Ruleset -> Html msg -> ( Dict String Ruleset, VirtualDom.Node msg )
render oldRulesetsByClassName styledHtmlNode =
    case styledHtmlNode of
        Text content ->
            ( oldRulesetsByClassName, VirtualDom.text content )

        VirtualDomNode node ->
            ( oldRulesetsByClassName, node )

        StyledHtmlNode tagName styledAttributes styledChildren ->
            let
                -- Styled attributes contain both actual virtual dom attributes and style rules.
                -- Here we get the actual virtual dom attributes and style rules from the styled attributes.
                foldAttribute attribute ( rulesetsByClassName, nodeProperties ) =
                    case attribute of
                        VirtualDomProperty property ->
                            ( rulesetsByClassName, property :: nodeProperties )

                        StyleAttribute rulesets ->
                            let
                                newRules =
                                    List.foldl (\ruleset d -> Dict.insert ruleset.className ruleset d) rulesetsByClassName rulesets

                                property =
                                    rulesets
                                        |> List.map .className
                                        |> String.join " "
                                        |> VanillaHtmlAttributes.class
                            in
                                ( newRules, property :: nodeProperties )

                ( partialRulesetsByClassName, nodeProperties ) =
                    List.foldr foldAttribute ( oldRulesetsByClassName, [] ) styledAttributes

                -- Styled html contains both actual virtual dom nodes and style rules
                foldChild styledChild ( rulesetsByClassName, nodeChildren ) =
                    let
                        ( newRules, nodeChild ) =
                            render rulesetsByClassName styledChild
                    in
                        ( newRules, nodeChild :: nodeChildren )

                ( newRulesetsByClassName, nodeChildren ) =
                    List.foldr foldChild ( partialRulesetsByClassName, [] ) styledChildren

                node =
                    VirtualDom.node tagName nodeProperties nodeChildren
            in
                ( newRulesetsByClassName, node )



-- program


type alias ProgramModel userModel msg =
    { viewAsVanillaHtml : VanillaHtml.Html msg
    , namesOfRulesetsAlreadyAdded : Set String
    , userModel : userModel
    }


makeStyles : Set String -> Dict String Ruleset -> ( Set String, List String )
makeStyles namesOfRulesetsAlreadyAdded rulesetsByClassName =
    let
        classNamesAndRulesToAdd =
            rulesetsByClassName
                |> Dict.toList
                |> List.filter (\( className, ruleset ) -> not <| Set.member className namesOfRulesetsAlreadyAdded)

        rulesToBeAdded =
            classNamesAndRulesToAdd
                |> List.map (\( className, ruleset ) -> ruleset.rules)

        namesOfRulesetsToBeAdded =
            classNamesAndRulesToAdd
                |> List.map Tuple.first
                |> Set.fromList
    in
        ( Set.union namesOfRulesetsToBeAdded namesOfRulesetsAlreadyAdded, rulesToBeAdded )


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

        ( namesOfAddedRulesets, rulesToBeAdded ) =
            makeStyles oldAddedCssSelectors style

        wrappingModel =
            { viewAsVanillaHtml = viewAsVanillaHtml
            , namesOfRulesetsAlreadyAdded = namesOfAddedRulesets
            , userModel = userModel
            }

        addStylesCmd =
            if rulesToBeAdded == [] then
                Cmd.none
            else
                addStyles rulesToBeAdded

        cmd =
            Cmd.batch [ addStylesCmd, userCmd ]
    in
        ( wrappingModel, cmd )
