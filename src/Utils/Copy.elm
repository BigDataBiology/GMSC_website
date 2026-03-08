module Utils.Copy exposing (CopyTarget(..), copyTextForTarget, copyTargetIsActive, markCopied, resetCopiedField, viewCopyFieldHeader)

{-| Shared clipboard-related helpers for sequence fields.

This keeps the Sequence and Cluster pages aligned on copy-button behavior,
active-state tracking, and header rendering.
-}

import Html exposing (Html, button, div, p, span)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)


{-| The copyable sequence fields currently supported by the UI. -}
type CopyTarget
    = ProteinSequence
    | NucleotideSequence


{-| Select the text corresponding to a copy target from a sequence-like record. -}
copyTextForTarget : CopyTarget -> { record | aa : String, nuc : String } -> String
copyTextForTarget target value =
    case target of
        ProteinSequence ->
            value.aa

        NucleotideSequence ->
            value.nuc


{-| Whether a given copy target is the one currently marked as copied. -}
copyTargetIsActive : CopyTarget -> Maybe CopyTarget -> Bool
copyTargetIsActive target copiedField =
    copiedField == Just target


{-| Mark one copy target as the most recently copied field. -}
markCopied : CopyTarget -> { model | copiedField : Maybe CopyTarget } -> { model | copiedField : Maybe CopyTarget }
markCopied target model =
    { model | copiedField = Just target }


{-| Clear the copied-state marker after loading a new record. -}
resetCopiedField : { model | copiedField : Maybe CopyTarget } -> { model | copiedField : Maybe CopyTarget }
resetCopiedField model =
    { model | copiedField = Nothing }


{-| Render the standard field header with the shared copy button affordance. -}
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
