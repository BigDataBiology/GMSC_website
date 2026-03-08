module Sequence exposing (Model, Msg(..), initialState, update, viewModel)

import Html
import Html exposing (Html, button, div, h1, h3, h4, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Browser.Navigation as Nav
import Char
import Http

import Json.Decode as D
import RemoteData exposing (WebData)
import Status

type alias Post = 
    { aa: String
    , habitat: String
    , nuc: String
    , seqid: String
    , tax: String
    }

type alias Model =
    { navKey : Nav.Key
    , post : WebData Post
    , copiedField : Maybe CopyTarget
    }

type CopyTarget
    = ProteinSequence
    | NucleotideSequence

postDecoder : D.Decoder Post
postDecoder =
    D.map5 Post
           (D.field "aminoacid" D.string)
           (D.field "habitat" D.string)
           (D.field "nucleotide" D.string)
           (D.field "seq_id" D.string)
           (D.field "taxonomy" D.string)

initialState : String -> Nav.Key -> (Model, Cmd Msg)
initialState seq_id navkey =
    ( { navKey = navkey
      , post = RemoteData.Loading
      , copiedField = Nothing
      }
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/seq-info/" ++ seq_id)
       , expect =  postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
    }
    )

type Msg
    = PostReceived (WebData Post)
    | CopyProtein
    | CopyNucleotide

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post, copiedField = Nothing }, Cmd.none )

        CopyProtein ->
            ( { model | copiedField = Just ProteinSequence }, Cmd.none )

        CopyNucleotide ->
            ( { model | copiedField = Just NucleotideSequence }, Cmd.none )

viewModel : Model-> Html.Html Msg
viewModel model =
    case model.post of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            Status.loading
                "Loading smORF record"
                "Fetching the selected 100AA entry, including its sequence and annotation."
        RemoteData.Success v ->
            viewSequencePage model v
        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)

viewSequencePage : Model -> Post -> Html Msg
viewSequencePage model v =
    div [ HtmlAttr.class "sequence-page" ]
        [ h1 [] [ text v.seqid ]
        , p [ HtmlAttr.class "sequence-subtitle" ]
            [ text "Summary of this 100AA smORF, including its amino acid sequence, nucleotide sequence, and ecological annotation." ]
        , div [ HtmlAttr.class "sequence-summary-grid" ]
            [ viewSequenceCard "Sequences"
                [ viewAminoAcidField "Protein sequence" v.aa (model.copiedField == Just ProteinSequence)
                , viewSequenceField "Nucleotide sequence" "cluster-sequence" v.nuc CopyNucleotide (model.copiedField == Just NucleotideSequence)
                ]
            , viewSequenceCard "Annotation"
                [ viewReadOnlyField "Taxonomic assignment" "" v.tax
                , viewReadOnlyField "Habitat" "" v.habitat
                ]
            ]
        ]

viewSequenceCard : String -> List (Html Msg) -> Html Msg
viewSequenceCard heading children =
    div [ HtmlAttr.class "cluster-card" ]
        (h4 [ HtmlAttr.class "cluster-card-title" ] [ text heading ] :: children)

viewReadOnlyField : String -> String -> String -> Html Msg
viewReadOnlyField label extraClass value =
    let
        valueClasses =
            String.join " " <| List.filter (\c -> c /= "") [ "cluster-value", extraClass ]
    in
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , p [ HtmlAttr.class valueClasses ] [ text value ]
        ]

viewSequenceField : String -> String -> String -> Msg -> Bool -> Html Msg
viewSequenceField label extraClass value copyMsg copied =
    let
        valueClasses =
            String.join " " <| List.filter (\c -> c /= "") [ "cluster-value", extraClass ]
    in
    div [ HtmlAttr.class "cluster-field" ]
        [ viewFieldHeader label copyMsg copied
        , p [ HtmlAttr.class valueClasses ] [ text value ]
        ]

viewAminoAcidField : String -> String -> Bool -> Html Msg
viewAminoAcidField label sequence copied =
    div [ HtmlAttr.class "cluster-field" ]
        [ viewFieldHeader label CopyProtein copied
        , div [ HtmlAttr.class "cluster-sequence-view" ]
            [ div [ HtmlAttr.class "cluster-value cluster-sequence cluster-aa-sequence" ]
                (List.map viewAminoAcidResidue (String.toList sequence))
            , p [ HtmlAttr.class "cluster-aa-legend-title" ] [ text "Legend" ]
            , div [ HtmlAttr.class "cluster-aa-legend" ]
                (List.map viewAminoAcidLegendItem aminoAcidLegend)
            ]
        ]

viewFieldHeader : String -> Msg -> Bool -> Html Msg
viewFieldHeader label copyMsg copied =
    div [ HtmlAttr.class "sequence-field-header" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
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

viewAminoAcidResidue : Char -> Html Msg
viewAminoAcidResidue residue =
    span [ HtmlAttr.class ("cluster-aa-residue " ++ aminoAcidGroupClass residue) ]
        [ text (String.fromChar residue) ]

viewAminoAcidLegendItem : ( String, String, String ) -> Html Msg
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

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
            
viewFetchError : String -> Html Msg
viewFetchError errorMessage =
    let
        errorHeading =
            "Couldn't fetch post at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]
