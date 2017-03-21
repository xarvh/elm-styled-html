module StyledHtml.Css
    exposing
        ( selector
        , andClass
        , makeClass
        , Class
        )

{-| This modules allows you to define and combine styles for Styled Html.


# Defining named classes
@docs Class, makeClass

# Combining selectors
@docs selector, andClass
-}

import FNV
import ParseInt
import String
import String.Extra
import StyledHtml.Private exposing (Rule, StyleSnippet)


{-| This describes a Styled Html class.
-}
type alias Class =
    StyledHtml.Private.Class


{-| TODO: implement ParseInt.toHex for speed and remove ParseInt dependency
-}
calculateHash =
    FNV.hashString >> ParseInt.toHex >> String.toLower


extendSelector left right =
    -- TODO: what if `rule` starts with an html tag name?
    --> Break the rule after the first continuous set of a-z and insert the class name there
    left ++ right


{-| Creates a new CSS rule (extending the parent rule) with the given CSS selector.

    buttonClass =
      StyledHtml.Css.makeClass "button"
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
selector : String -> List StyleSnippet -> List (List Rule) -> List Rule
selector selector styleSnippets compositeRules =
    let
        unscopedRules =
            { selector = "", styleSnippets = styleSnippets } :: List.concat compositeRules

        scopedRules =
            unscopedRules
                |> List.map (\rule -> { rule | selector = extendSelector selector rule.selector })
    in
        scopedRules


{-| This is used to select a Styled Html Class.

    buttonClass =
        StyledHtml.Css.makeClass "button" [] []

    inputClass =
        StyledHtml.Css.makeClass "input"
          [ "border: 1px solid #b1b1b1" ]
          [ StyledHtml.Css.andClass buttonClass
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
andClass : Class -> List StyleSnippet -> List (List Rule) -> List Rule
andClass class styleSnippets compositeRules =
    selector ("." ++ class.name) styleSnippets compositeRules


{-| Defines a new styled html `Class`.

The first argument is the name to use as a base for the class name.
Under the hood, this base name will be extended with a hash string of the class rules.

The second argument is a list of style attributes to apply directly to the class, and
the third argument can be used to make composite selections.

    modalContainerClass =
      StyledHtml.Css.makeClass "modal-container"
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
makeClass : String -> List StyleSnippet -> List (List Rule) -> Class
makeClass rawClassName styleSnippets compositeRules =
    let
        -- TODO: ensure it does not make sense as a CSS selector
        classNamePlaceholder =
            "@@classname@@"

        rulesWithPlaceholder =
            selector ("." ++ classNamePlaceholder) styleSnippets compositeRules

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
