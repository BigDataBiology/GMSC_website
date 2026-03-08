module Utils.Sequences exposing (aminoAcidGroupClass, viewAminoAcidSequence)

import Char
import Html exposing (Html, div, p, span, text)
import Html.Attributes as HtmlAttr


viewAminoAcidSequence : String -> Html msg
viewAminoAcidSequence sequence =
    div [ HtmlAttr.class "cluster-sequence-view" ]
        [ div [ HtmlAttr.class "cluster-value cluster-sequence cluster-aa-sequence" ]
            (List.map viewAminoAcidResidue (String.toList sequence))
        , p [ HtmlAttr.class "cluster-aa-legend-title" ] [ text "Legend" ]
        , div [ HtmlAttr.class "cluster-aa-legend" ]
            (List.map viewAminoAcidLegendItem aminoAcidLegend)
        ]


viewAminoAcidResidue : Char -> Html msg
viewAminoAcidResidue residue =
    span [ HtmlAttr.class ("cluster-aa-residue " ++ aminoAcidGroupClass residue) ]
        [ text (String.fromChar residue) ]


viewAminoAcidLegendItem : ( String, String, String ) -> Html msg
viewAminoAcidLegendItem ( groupLabel, residues, className ) =
    div [ HtmlAttr.class "cluster-aa-legend-item" ]
        [ span [ HtmlAttr.class ("cluster-aa-legend-swatch " ++ className) ] [ text "A" ]
        , span [ HtmlAttr.class "cluster-aa-legend-text" ]
            [ text (groupLabel ++ " (" ++ residues ++ ")") ]
        ]


aminoAcidLegend : List ( String, String, String )
aminoAcidLegend =
    [ ( "Hydrophobic", "A, V, I, L, M", "aa-hydrophobic" )
    , ( "Aromatic", "F, W, Y", "aa-aromatic" )
    , ( "Polar uncharged", "S, T, N, Q, C", "aa-polar" )
    , ( "Positive", "K, R, H", "aa-basic" )
    , ( "Negative", "D, E", "aa-acidic" )
    , ( "Special", "G, P", "aa-special" )
    , ( "Other / unknown", "X, U, O, *", "aa-other" )
    ]


aminoAcidGroupClass : Char -> String
aminoAcidGroupClass residue =
    case Char.toUpper residue of
        'A' ->
            "aa-hydrophobic"

        'V' ->
            "aa-hydrophobic"

        'I' ->
            "aa-hydrophobic"

        'L' ->
            "aa-hydrophobic"

        'M' ->
            "aa-hydrophobic"

        'F' ->
            "aa-aromatic"

        'W' ->
            "aa-aromatic"

        'Y' ->
            "aa-aromatic"

        'S' ->
            "aa-polar"

        'T' ->
            "aa-polar"

        'N' ->
            "aa-polar"

        'Q' ->
            "aa-polar"

        'C' ->
            "aa-polar"

        'K' ->
            "aa-basic"

        'R' ->
            "aa-basic"

        'H' ->
            "aa-basic"

        'D' ->
            "aa-acidic"

        'E' ->
            "aa-acidic"

        'G' ->
            "aa-special"

        'P' ->
            "aa-special"

        _ ->
            "aa-other"
