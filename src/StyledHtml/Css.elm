module StyledHtml.Css
    exposing
        ( selector
        , andRuleset
        , makeRuleset
        , Ruleset
        )

{-| This modules allows you to define and combine styles for Styled Html.


# Defining named classes
@docs Ruleset, makeRuleset

# Combining selectors
@docs selector, andRuleset
-}

import String
import String.Extra
import StyledHtml.Private exposing (Rule, Declaration)


{-| This describes a Styled Html class.
-}
type alias Ruleset =
    StyledHtml.Private.Ruleset




extendSelector left right =
    -- TODO: what if `rule` starts with an html tag name?
    --> Break the rule after the first continuous set of a-z and insert the class name there
    left ++ right


{-| Creates a new CSS rule (extending the parent rule) with the given CSS selector.

    buttonRuleset =
      StyledHtml.Css.makeRuleset "button"
        [ "border: 2px solid grey" ]
        [ StyledHtml.Css.selector ":hover"
          [ "background-color: grey"
          , "border: 2px solid black"
          ]
          [ StyledHtml.Css.selector " svg.icon"
            [ "fill: red" ]
            []
          ]
        ]

will result in these rules:

    .button_37A2B {
      border: 2px solid black;
    }

    .button_37A2B:hover {
      background-color: grey;
      border: 2px solid black;
    }

    .button_37A2B:hover svg.icon {
      fill: red;
    }
-}
selector : String -> List Declaration -> List (List Rule) -> List Rule
selector selector declarations compositeRules =
    let
        unscopedRules =
            { selector = "", declarations = declarations } :: List.concat compositeRules

        scopedRules =
            unscopedRules
                |> List.map (\rule -> { rule | selector = extendSelector selector rule.selector })
    in
        scopedRules


{-| This is used to select a Styled Html Ruleset.

    buttonRuleset =
        StyledHtml.Css.makeRuleset "button" [] []

    inputRuleset =
        StyledHtml.Css.makeRuleset "input"
          [ "border: 1px solid #b1b1b1" ]
          [ StyledHtml.Css.andRuleset buttonRuleset
            [ "border: 2px solid #b1b1b1"
            , "cursor: pointer"
            ]
            []
          ]

will produce the rules:

    .button_932D {
    }

    .input_21F1 {
      border: 1px solid #b1b1b1;
    }

    .input_21F1.button_932D {
      border: 2px solid #b1b1b1;
      cursor: pointer;
    }
-}
andRuleset : Ruleset -> List Declaration -> List (List Rule) -> List Rule
andRuleset ruleset declarations compositeRules =
    selector ("." ++ ruleset.className) declarations compositeRules


{-| Defines a new styled html `Ruleset`.

The first argument is the name to use as a base for the class name.
Under the hood, this base name will be extended with a hash string of the class rules.

The second argument is a list of style attributes to apply directly to the class, and
the third argument can be used to make composite selections.

    modalContainerRuleset =
      StyledHtml.Css.makeRuleset "modal-container"
        [ "display: flex"
        , "justify-content: center"
        ]
        [ StyledHtml.Css.selector " .close-icon"
            [ "width: 12px" ]
            []
        ]

will produce the rules:

    .modal-container_D783 {
        display: flex;
        justify-content: center;
    }

    .modal-container_D783 .close-icon {
        width: 12px;
    }
-}
makeRuleset : String -> List Declaration -> List (List Rule) -> Ruleset
makeRuleset className declarations compositeRules =
    let
        declarationToString declaration =
            "  " ++ declaration ++ ";\n"

        declarationsToString rules =
            rules
                |> List.map declarationToString
                |> String.join ""

        ruleToString rule =
             rule.selector ++ " {\n" ++ declarationsToString rule.declarations ++ "}\n"

        rules =
            selector ("." ++ className) declarations compositeRules

        rulesAsSingleString =
            rules
                |> List.map ruleToString
                |> String.join "\n"
    in
        { className = className
        , rules = rulesAsSingleString
        }
