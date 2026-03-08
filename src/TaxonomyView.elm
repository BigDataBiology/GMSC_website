module TaxonomyView exposing (view)

import Html exposing (Html, span, text)
import Html.Attributes as HtmlAttr
import List


view : String -> Html msg
view taxonomy =
    let
        items =
            taxonomy
                |> String.split ";"
                |> List.filter (\item -> not (String.isEmpty item))
                |> List.map viewTaxon
                |> List.intersperse (span [ HtmlAttr.class "taxonomy-separator" ] [ text " > " ])
    in
    span [ HtmlAttr.class "taxonomy-display" ] items


viewTaxon : String -> Html msg
viewTaxon name =
    let
        ( rank, label ) =
            splitTaxon name

        displayLabel =
            if String.isEmpty label then
                "unnamed"
            else
                label
    in
    span [ HtmlAttr.class "taxonomy-segment" ]
        [ span [ HtmlAttr.class "taxonomy-name" ] [ text displayLabel ]
        , span [ HtmlAttr.class "taxonomy-class" ] [ text (" (" ++ rank ++ ")") ]
        ]


splitTaxon : String -> ( String, String )
splitTaxon name =
    let
        parts =
            String.split "__" name

        rank =
            case parts of
                [] ->
                    "unknown"

                prefix :: _ ->
                    case prefix of
                        "r" ->
                            "root"

                        "d" ->
                            "domain"

                        "k" ->
                            "kingdom"

                        "p" ->
                            "phylum"

                        "c" ->
                            "class"

                        "o" ->
                            "order"

                        "f" ->
                            "family"

                        "g" ->
                            "genus"

                        "s" ->
                            "species"

                        _ ->
                            "unknown"

        label =
            case parts of
                [ _, rawLabel ] ->
                    String.replace "_" " " rawLabel

                _ ->
                    name
    in
    ( rank, label )
