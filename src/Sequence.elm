module Sequence exposing (Model, Msg(..), copyTargetForMsg, initialState, update, viewModel)

import Html
import Html exposing (Html, div, h1, h3, h4, p, text)
import Html.Attributes as HtmlAttr
import Browser.Navigation as Nav
import Http

import Json.Decode as D
import RemoteData exposing (WebData)
import Status
import TaxonomyView
import Utils.Copy as Copy exposing (CopyTarget(..))
import Utils.ResultsPipeline exposing (httpErrorMessage)
import Utils.Sequences

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
            ( Copy.resetCopiedField { model | post = post }, Cmd.none )

        CopyProtein ->
            ( Copy.markCopied ProteinSequence model, Cmd.none )

        CopyNucleotide ->
            ( Copy.markCopied NucleotideSequence model, Cmd.none )

copyTargetForMsg : Msg -> Maybe CopyTarget
copyTargetForMsg msg =
    case msg of
        CopyProtein ->
            Just ProteinSequence

        CopyNucleotide ->
            Just NucleotideSequence

        _ ->
            Nothing

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
            viewFetchError (httpErrorMessage httpError)

viewSequencePage : Model -> Post -> Html Msg
viewSequencePage model v =
    div [ HtmlAttr.class "sequence-page" ]
        [ h1 [] [ text v.seqid ]
        , p [ HtmlAttr.class "sequence-subtitle" ]
            [ text "Summary of this 100AA smORF, including its amino acid sequence, nucleotide sequence, and ecological annotation." ]
        , div [ HtmlAttr.class "context-callout" ]
            [ p [ HtmlAttr.class "context-copy" ]
                [ text "This is a 100AA catalogue entry: a non-redundant smORF accession after collapsing exact amino acid duplicates. Here, 100AA refers to the identity threshold used to build the catalogue, not to sequence length." ]
            , p [ HtmlAttr.class "context-copy" ]
                [ text "Taxonomy and habitat summarise the source contigs, metagenomes, or isolate genomes used to annotate this smORF in GMSC." ]
            ]
        , div [ HtmlAttr.class "sequence-summary-grid" ]
            [ viewSequenceCard "Sequences"
                [ viewAminoAcidField ("Protein sequence ("++String.fromInt (String.length v.aa)++" amino acids)") v.aa (Copy.copyTargetIsActive ProteinSequence model.copiedField)
                , viewSequenceField "Nucleotide sequence" "cluster-sequence" v.nuc CopyNucleotide (Copy.copyTargetIsActive NucleotideSequence model.copiedField)
                ]
            , viewSequenceCard "Annotation"
                [ viewTaxonomyField "Taxonomic assignment" v.tax
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
        [ Copy.viewCopyFieldHeader label copyMsg copied
        , p [ HtmlAttr.class valueClasses ] [ text value ]
        ]

viewTaxonomyField : String -> String -> Html Msg
viewTaxonomyField label taxonomy =
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , div [ HtmlAttr.class "cluster-value" ] [ TaxonomyView.view taxonomy ]
        ]

viewAminoAcidField : String -> String -> Bool -> Html Msg
viewAminoAcidField label sequence copied =
    div [ HtmlAttr.class "cluster-field" ]
        [ Copy.viewCopyFieldHeader label CopyProtein copied
        , Utils.Sequences.viewAminoAcidSequence sequence
        ]

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
