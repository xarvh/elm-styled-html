module StyledHtml.Css exposing (..)

import FNV
import ParseInt
import String
import String.Extra
import StyledHtml
    exposing
        ( Class
        , Rule
        , StyleSnippet
        )


extendSelector left right =
    -- TODO: what if `rule` starts with an html tag name?
    --> Break the rule after the first continuous set of a-z and insert the class name there
    left ++ right


{-|
  Takes a styleSnippets list and rules and puts all of them under the provided selector
  TODO: this function should be exposed as something like "andSelector ..."
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


andClass : Class -> List StyleSnippet -> List (List Rule) -> List Rule
andClass class styleSnippets compositeRules =
    selector ("." ++ class.name) styleSnippets compositeRules


hover : List StyleSnippet -> List (List Rule) -> List Rule
hover styleSnippets compositeRules =
    selector ":hover" styleSnippets compositeRules



{-| TODO: implement ParseInt.toHex for speed and remove ParseInt dependency
-}
calculateHash =
    FNV.hashString >> ParseInt.toHex >> String.toLower


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
