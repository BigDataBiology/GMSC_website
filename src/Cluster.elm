module Cluster exposing (ClusterPost(..), Model, Msg(..), copyTargetForMsg, initialState, update, viewModel)

import Html
import Html exposing (Html, div, h1, h4, p, span, text)
import Html.Attributes as HtmlAttr
import Browser.Navigation as Nav
import Http

import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Popover as Popover
import Json.Decode as D
import Members
import Status
import TaxonomyView
import Utils.Copy as Copy exposing (CopyTarget(..))
import Utils.Sequences


qualityString : Quality -> String
qualityString q =
    if q.antifam && q.terminal && q.rnacode < 0.05 && (q.metat > 1 || q.riboseq > 1 || q.metap > 0.5) then
      "High quality"
    else
      "Not high quality"

rnaCodeString : Float -> String
rnaCodeString n =
  if n == 2 then
    "Not performed (not enough smORFs in family)"
  else if n < 0.05 then
      "✔ Passed (p-value: " ++ (String.fromFloat n) ++ ")"
  else
      "✘ Not passed (p-value" ++ (String.fromFloat n) ++ ")"

passOrFail : Bool -> String
passOrFail value =
  if value then
    "✔ Passed"
  else
    "✘ Failed"

metaTRString : Int -> String
metaTRString n =
  if n > 1 then
    "✔ Passed (detected in " ++ (String.fromInt n) ++ " samples)"
  else if n == 1 then
    "✘ Not passed (detected in only 1 sample)"
  else
    "✘ Not detected"

metaPString : Float -> String
metaPString n =
    (if n > 0.5 then "✔ Passed " else "✗ Not passed") ++ "(coverage of smORF on proteomics: " ++ (String.fromFloat (n * 100.0) |> String.left 5) ++ "%)"

qualityBadgeClass : Quality -> String
qualityBadgeClass q =
    if qualityString q == "High quality" then
        "cluster-quality-badge is-high"
    else
        "cluster-quality-badge is-standard"

type alias Quality =
    { antifam: Bool
    , metap: Float
    , metat: Int
    , riboseq : Int
    , rnacode : Float
    , terminal : Bool
    }

type alias Cluster =
    { aa: String
    , habitat: String
    , nuc: String
    , seqid: String
    , tax: String
    , quality: Quality
    }

type ClusterPost =
    Loading
    | LoadError String
    | Loaded Cluster

type alias Model =
    { clusterpost : ClusterPost
    , memberpost : Members.Model
    , membersRequested : Bool
    , showQualityDetails : Bool
    , copiedField : Maybe CopyTarget
    , navKey : Nav.Key
    , popoverState1 : Popover.State
    }

type APIResult =
    APIError String
    | APIResultOK { aa: String
                  , habitat: String
                  , nuc: String
                  , seqid: String
                  , tax: String
                  , quality: Quality
                  }

type Msg
    = ResultsData ( Result Http.Error APIResult )
    | ShowMembers
    | ShowQuality
    | CopyProtein
    | CopyNucleotide
    | MembersMsg Members.Msg
    | PopoverMsg1 Popover.State

decodeQuality : D.Decoder Quality
decodeQuality =
    D.map6 Quality
        (D.field "antifam" D.bool)
        (D.field "metap" D.float)
        (D.field "metat" D.int)
        (D.field "riboseq" D.int)
        (D.field "rnacode" D.float)
        (D.field "terminal" D.bool)

decodeSeqInfo : D.Decoder APIResult
decodeSeqInfo =
    let
        bAPIResultOK a h n s t q = APIResultOK { aa = a, habitat = h, nuc = n, seqid = s, tax = t, quality = q}
    in D.map6 bAPIResultOK
        (D.field "aminoacid" D.string)
        (D.field "habitat" D.string)
        (D.field "nucleotide" D.string)
        (D.field "seq_id" D.string)
        (D.field "taxonomy" D.string)
        (D.field "quality" decodeQuality)


initialState : String -> Nav.Key -> (Model, Cmd Msg)
initialState seq_id navkey =
    ( { clusterpost = Loading
    , memberpost = Members.loadingModel
    , membersRequested = False
    , showQualityDetails = False
    , copiedField = Nothing
    , navKey = navkey
    , popoverState1 = Popover.initialState
    }
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/seq-info/" ++ seq_id)
    , expect = Http.expectJson ResultsData decodeSeqInfo
    }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultsData r -> case r of
            Ok (APIResultOK v) -> ( Copy.resetCopiedField { model | clusterpost = Loaded v }, Cmd.none )
            Ok (APIError e) -> ( { model | clusterpost = LoadError e}, Cmd.none )
            Err err -> case err of
                Http.BadUrl s -> ({ model | clusterpost = LoadError ("Bad URL: "++ s)}, Cmd.none)
                Http.Timeout  -> ({ model | clusterpost = LoadError ("Timeout")} , Cmd.none)
                Http.NetworkError -> ({ model | clusterpost = LoadError ("Network error!") }, Cmd.none)
                Http.BadStatus s -> ({ model | clusterpost = LoadError (("Bad status: " ++ String.fromInt s)) } , Cmd.none)
                Http.BadBody s -> ({ model | clusterpost = LoadError (("Bad body: " ++ s)) }  , Cmd.none)

        ShowMembers ->
            case model.clusterpost of
              Loaded hm->
                let
                  (sm, cmd) = Members.initialState hm.seqid
                in ( { model | memberpost = sm, membersRequested = True }, Cmd.map MembersMsg cmd )
              _ ->
                (model,Cmd.none)

        ShowQuality ->
            ( {model | showQualityDetails = not model.showQualityDetails}, Cmd.none )

        CopyProtein ->
            ( Copy.markCopied ProteinSequence model, Cmd.none )

        CopyNucleotide ->
            ( Copy.markCopied NucleotideSequence model, Cmd.none )

        MembersMsg m ->
            let
                (nqm, cmd) = Members.update m model.memberpost
            in
                ( { model | memberpost = nqm}, Cmd.map MembersMsg cmd )

        PopoverMsg1 state ->
            ( { model | popoverState1 = state }, Cmd.none )

copyTargetForMsg : Msg -> Maybe CopyTarget
copyTargetForMsg msg =
    case msg of
        CopyProtein ->
            Just ProteinSequence

        CopyNucleotide ->
            Just NucleotideSequence

        _ ->
            Nothing

viewModel : Model -> Html Msg
viewModel model =
    case model.clusterpost of
        Loading ->
            Status.loading
                "Loading cluster summary"
                "Fetching the selected 90AA cluster, including consensus sequences, annotations, and quality evidence."
        LoadError e ->
            div []
                [ text "Error "
                , text e
                ]
        Loaded v ->
            viewClusterPage model v

viewClusterPage : Model -> Cluster -> Html Msg
viewClusterPage model v =
    div [ HtmlAttr.class "cluster-page" ]
        [ h1 [] [ text v.seqid ]
        , p [ HtmlAttr.class "cluster-subtitle" ]
            [ text "Summary of this 90AA cluster, including consensus sequences, ecological annotation, quality evidence, and linked 100AA members." ]
        , div [ HtmlAttr.class "context-callout" ]
            [ p [ HtmlAttr.class "context-copy" ]
                [ text "This is a 90AA family-level cluster. In GMSC, 90AA and 100AA refer to catalogue identity thresholds rather than sequence length: 90AA groups related smORFs at 90% amino acid identity, while 100AA accessions are non-redundant sequence entries." ]
            , p [ HtmlAttr.class "context-copy" ]
                [ text "The linked 100AA members let you inspect the individual non-redundant sequences assigned to this family, while the taxonomy and habitat summarise the ecological context of the cluster." ]
            ]
        , div [ HtmlAttr.class "cluster-summary-grid" ]
            [ viewCard "Consensus sequences"
                [ viewAminoAcidField "Consensus protein sequence" v.aa (Copy.copyTargetIsActive ProteinSequence model.copiedField)
                , viewSequenceField "Consensus nucleotide sequence" "cluster-sequence" v.nuc CopyNucleotide (Copy.copyTargetIsActive NucleotideSequence model.copiedField)
                ]
            , viewCard "Annotations"
                [ viewTaxonomyField "Taxonomic assignment" v.tax
                , viewField "Habitat" "" v.habitat
                ]
            , viewQualityCard v model.showQualityDetails model.popoverState1
            ]

        , viewMembersSection model
        ]

viewCard : String -> List (Html Msg) -> Html Msg
viewCard heading children =
    div [ HtmlAttr.class "cluster-card" ]
        (h4 [ HtmlAttr.class "cluster-card-title" ] [ text heading ] :: children)

viewField : String -> String -> String -> Html Msg
viewField label extraClass value =
    let
        valueClasses =
            String.join " " <| List.filter (\c -> c /= "") [ "cluster-value", extraClass ]
    in
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , p [ HtmlAttr.class valueClasses ] [ text value ]
        ]

viewTaxonomyField : String -> String -> Html Msg
viewTaxonomyField label taxonomy =
    div [ HtmlAttr.class "cluster-field" ]
        [ p [ HtmlAttr.class "cluster-label" ] [ text label ]
        , div [ HtmlAttr.class "cluster-value" ] [ TaxonomyView.view taxonomy ]
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

viewAminoAcidField : String -> String -> Bool -> Html Msg
viewAminoAcidField label sequence copied =
    div [ HtmlAttr.class "cluster-field" ]
        [ Copy.viewCopyFieldHeader label CopyProtein copied
        , Utils.Sequences.viewAminoAcidSequence sequence
        ]

viewQualityCard : Cluster -> Bool -> Popover.State -> Html Msg
viewQualityCard v showDetails pop =
    div [ HtmlAttr.class "cluster-card cluster-card-wide" ]
        [ h4 [ HtmlAttr.class "cluster-card-title cluster-card-title-inline" ]
            [ text "Quality overview"
            , Popover.config
                (Button.button
                    [ Button.small
                    , Button.outlineInfo
                    , Button.attrs <| Popover.onHover pop PopoverMsg1
                    ]
                    [ span [ HtmlAttr.class "fa fa-question-circle" ] [] ]
                )
                |> Popover.right
                |> Popover.content []
                    [ text "High quality is defined as: Pass all in silico quality tests (Not in Antifam database, Pass the terminal checking, P-value of RNAcode < 0.05) and contained at least one item of experimental evidence (Align to at least 2 metatranscriptomic samples, alignment to at least 2 Riboseq samples, or the coverage of metaproteomic peptides on small protein sequences >0.5)" ]
                |> Popover.view pop
            ]
        , div [ HtmlAttr.class (qualityBadgeClass v.quality) ] [ text (qualityString v.quality) ]
        , p [ HtmlAttr.class "cluster-quality-summary" ]
            [ text "Review the detailed quality checks below to understand the evidence supporting this cluster." ]
        , if showDetails then
            div []
                [ viewQualityDetails v
                , div [ HtmlAttr.class "cluster-actions" ]
                    [ Button.button [ Button.outlineInfo, Button.onClick ShowQuality ] [ text "Hide detailed quality information" ] ]
                ]
          else
            div [ HtmlAttr.class "cluster-actions" ]
                [ Button.button [ Button.info, Button.onClick ShowQuality ] [ text "Show detailed quality information" ] ]
        ]

viewMembersSection : Model -> Html Msg
viewMembersSection model =
    div [ HtmlAttr.class "cluster-members-section" ]
        [ h4 [ HtmlAttr.class "cluster-section-title" ] [ text "Cluster members" ]
        , p [ HtmlAttr.class "cluster-section-copy" ] [ text "Load the 100AA non-redundant smORF accessions assigned to this 90AA family to inspect their sequences, habitats, and taxonomy." ]
        , if model.membersRequested then
            Members.viewModel model.memberpost
                |> Html.map MembersMsg
          else
            div [ HtmlAttr.class "cluster-actions" ]
                [ Button.button [ Button.info, Button.onClick ShowMembers ] [ text "Load cluster members" ] ]
        ]

viewQualityDetails : Cluster -> Html Msg
viewQualityDetails v =
  div [ HtmlAttr.class "cluster-quality-grid" ]
      [ viewField "Antifam" "" (passOrFail v.quality.antifam)
      , viewField "Terminal checking" "" (passOrFail v.quality.terminal)
      , viewField "RNAcode" "" (rnaCodeString v.quality.rnacode)
      , viewField "metaTranscriptome" "" (metaTRString v.quality.metat)
      , viewField "Riboseq" "" (metaTRString v.quality.riboseq)
      , viewField "metaProteome" "" (metaPString v.quality.metap)
      ]
