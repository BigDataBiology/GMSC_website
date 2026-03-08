module Sequence exposing (Model, Msg(..), initialState, update, viewModel)

import Html
import Html exposing (Html, div, h1, h3, h4, p, span, text)
import Html.Attributes as HtmlAttr
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
    }

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
      }
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/seq-info/" ++ seq_id)
       , expect =  postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
    }
    )

type Msg
    = PostReceived (WebData Post)

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

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
            viewSequencePage v
        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)

viewSequencePage : Post -> Html Msg
viewSequencePage v =
    div [ HtmlAttr.class "sequence-page" ]
        [ h1 [] [ text v.seqid ]
        , p [ HtmlAttr.class "sequence-subtitle" ]
            [ text "Summary of this 100AA smORF, including its amino acid sequence, nucleotide sequence, and ecological annotation." ]
        , div [ HtmlAttr.class "sequence-summary-grid" ]
            [ viewSequenceCard "Sequences"
                [ viewAminoAcidField "Protein sequence" v.aa
                , viewSequenceField "Nucleotide sequence" "cluster-sequence" v.nuc
                ]
            , viewSequenceCard "Annotation"
                [ viewSequenceField "Taxonomic assignment" "" v.tax
                , viewSequenceField "Habitat" "" v.habitat
                ]
            ]
        ]

viewSequenceCard : String -> List (Html Msg) -> Html Msg
viewSequenceCard heading children =
    div [ HtmlAttr.class "cluster-card" ]
        (h4 [ HtmlAttr.class "cluster-card-title" ] [ text heading ] :: children)

viewSequenceField : String -> String -> String -> Html Msg
viewSequenceField label extraClass value =
    let
        valueClasses =
            String.join " " <| List.filter (\c -> c /= "") [ "cluster-value", extraClass ]
    in
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , p [ HtmlAttr.class valueClasses ] [ text value ]
        ]

viewAminoAcidField : String -> String -> Html Msg
viewAminoAcidField label sequence =
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , div [ HtmlAttr.class "cluster-sequence-view" ]
            [ div [ HtmlAttr.class "cluster-value cluster-sequence cluster-aa-sequence" ]
                (List.map viewAminoAcidResidue (String.toList sequence))
            , p [ HtmlAttr.class "cluster-aa-legend-title" ] [ text "Legend" ]
            , div [ HtmlAttr.class "cluster-aa-legend" ]
                (List.map viewAminoAcidLegendItem aminoAcidLegend)
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
