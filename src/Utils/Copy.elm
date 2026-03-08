module Utils.Copy exposing (CopyTarget(..), copyTextForTarget, copyTargetIsActive, markCopied, resetCopiedField, viewCopyFieldHeader)

import Html exposing (Html, button, div, p, span)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)


type CopyTarget
    = ProteinSequence
    | NucleotideSequence


copyTextForTarget : CopyTarget -> { record | aa : String, nuc : String } -> String
copyTextForTarget target value =
    case target of
        ProteinSequence ->
            value.aa

        NucleotideSequence ->
            value.nuc


copyTargetIsActive : CopyTarget -> Maybe CopyTarget -> Bool
copyTargetIsActive target copiedField =
    copiedField == Just target


markCopied : CopyTarget -> { model | copiedField : Maybe CopyTarget } -> { model | copiedField : Maybe CopyTarget }
markCopied target model =
    { model | copiedField = Just target }


resetCopiedField : { model | copiedField : Maybe CopyTarget } -> { model | copiedField : Maybe CopyTarget }
resetCopiedField model =
    { model | copiedField = Nothing }


viewCopyFieldHeader : String -> msg -> Bool -> Html msg
viewCopyFieldHeader label copyMsg copied =
    div [ HtmlAttr.class "sequence-field-header" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ Html.text label ]
        , button
            [ HtmlAttr.class
                (if copied then
                    "sequence-copy-button is-copied"
                 else
                    "sequence-copy-button"
                )
            , HtmlAttr.type_ "button"
            , HtmlAttr.title
                (if copied then
                    "Copied"
                 else
                    "Copy sequence"
                )
            , HtmlAttr.attribute "aria-label"
                (if copied then
                    "Copied to clipboard"
                 else
                    "Copy sequence to clipboard"
                )
            , onClick copyMsg
            ]
            [ span
                [ HtmlAttr.class
                    (if copied then
                        "fa fa-check"
                     else
                        "fa fa-copy"
                    )
                ]
                []
            ]
        ]
