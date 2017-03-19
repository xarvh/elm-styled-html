module StyledHtml exposing (..)

import Html


type alias StyleSnippet =
    String


type alias Rule =
    { selector : String
    , styleSnippets : List StyleSnippet
    }


type alias Class =
    { name : String
    , rules : List Rule
    }


extendSelectorWithClassName className selector =
    -- TODO: what if `rule` starts with an html tag name?
    --> Break the rule after the first continuous set of a-z and insert the class name there
    "." ++ className ++ selector


{-|
  Takes a styleSnippets list and rules and puts all of them under the provided selector
  TODO: this function should be exposed as something like "andSelector ..."
-}
pushScope : String -> List StyleSnippet -> List Rule -> List Rule
pushScope selector styleSnippets compositeRules =
    let
        unscopedRules =
            Rule "" styleSnippets :: rules

        scopedRules =
            unscopedRules
                |> List.map (\rule -> { rule | selector = extendSelector selector rule.selector })
    in
        scopedRules


andClass : Class -> List StyleSnippet -> List Rule -> List Rule
andClass class styleSnippets compositeRules =
    pushScope ("." ++ class.name) styleSnippets compositeRules


hover : List StyleSnippet -> List Rule -> List Rule
hover styleSnippets rules =
    pushScope ":hover" styleSnippets compositeRules


{-| TODO: use an actual hash function
-}
calculateHash =
    String.length >> toString


{-|
  Ideally this function is called only once at start up, so we want to
  put here most of the crunching and processing.
-}
makeClass : String -> List StyleSnippet -> List Rule -> Class
makeClass rawClassName styleSnippets compositeRules =
    let
        -- TODO: ensure it does not make sense as a CSS selector
        classNamePlaceholder =
            "@@classname@@"

        rulesWithPlaceholder =
            andClass classNamePlaceholder styleSnippets rules

        -- TODO: This should be a configured option
        hash =
            -- TODO: properly convert the rules to a string?
            calculateHash (toString rules)

        -- TODO: the function to turn raw class name and hash into actual class name should be a configured option?
        finalClassName =
            rawClassName ++ "_" ++ hash

        snippetToString snippet =
            "  " ++ snippet ++ ";\n"

        ruleToString rule =
            String.Extra.replace classNamePlaceholder finalClassName rule.selector ++ " {\n" ++ List.map snippetToString rule.styleSnippets ++ "}\n"

        actualRules =
            rulesWithPlaceholder
                |> List.map ruleToString
                |> String.join "\n"
    in
        { name = finalClassName
        , rules = actualRules
        }


--

type Attribute
  = HtmlAttribute Html.Attribute
  | StyleAttribute Class

type Node
  = Node String (List Attribute) (List Node)
  | Text String



text : String -> Node
text content =
  Text content


node  : String -> List Attribute -> List Node -> Node
node tag attributes children =
  Node tag attributes children



useClass : Class -> Attribute
useClass class =
  StyleAttribute class




--


render : Dict String -> Node msg -> Dict
render oldRulesBySelector styledHtmlNode =
  case styledHtmlNode of
    Text content ->
      ( [], Html.text content )

    Node tagName attributes children ->


    let



        foldAttributes attr (attrs, rules) =
          -- TODO case attr of HtmlAttribute | StyleAttribute
          ( [], rules )



        (htmlAttributes, rules) =
          List.foldr foldAttributes ([], []) styledHtml.attributes


        (htmlAttributes, styledHtmlAttributes) =
          33




        rulesBySelector =
            Dict.empty



        (html, rulesBySelector) =
          let
            children =

            attributes =

            styledHtml.constructor
          in


        children =
          List.map (render rulesBySelector)

        styledHtml.constructor







    in
        ( "", Html.text "" )






getContentAndStyle : Node msg -> ( Html msg, String )
getContentAndStyle styledHtml =
  let
      (html, rulesByselector) =
        render Dict.empty styledHtml
  in
      (html, toString rulesBySelector )
