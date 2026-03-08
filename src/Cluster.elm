module Cluster exposing (ClusterPost(..), Model, Msg(..), initialState, update, viewModel)

import Html
import Html exposing (Html, button, div, h1, h4, p, span, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Browser.Navigation as Nav
import Char
import Http

import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Json.Decode as D
import Members
import Status
import TaxonomyView


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
    , ask: Bool
    , showQualityDetails : Bool
    , copiedField : Maybe CopyTarget
    , navKey : Nav.Key
    , popoverState1 : Popover.State
    }

type CopyTarget
    = ProteinSequence
    | NucleotideSequence

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
    | Showmember
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

decodeAPIResult : D.Decoder APIResult
decodeAPIResult =
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
    , memberpost = { memberpost = Members.MLoading
                   , showpost = Members.SLoading
                   , page = 1
                   , myDrop1State = Dropdown.initialState
                   }
    , ask = False
    , showQualityDetails = False
    , copiedField = Nothing
    , navKey = navkey
    , popoverState1 = Popover.initialState
    }
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/seq-info/" ++ seq_id)
    , expect = Http.expectJson ResultsData decodeAPIResult
    }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultsData r -> case r of
            Ok (APIResultOK v) -> ( { model | clusterpost = Loaded v, copiedField = Nothing }, Cmd.none )
            Ok (APIError e) -> ( { model | clusterpost = LoadError e}, Cmd.none )
            Err err -> case err of
                Http.BadUrl s -> ({ model | clusterpost = LoadError ("Bad URL: "++ s)}, Cmd.none)
                Http.Timeout  -> ({ model | clusterpost = LoadError ("Timeout")} , Cmd.none)
                Http.NetworkError -> ({ model | clusterpost = LoadError ("Network error!") }, Cmd.none)
                Http.BadStatus s -> ({ model | clusterpost = LoadError (("Bad status: " ++ String.fromInt s)) } , Cmd.none)
                Http.BadBody s -> ({ model | clusterpost = LoadError (("Bad body: " ++ s)) }  , Cmd.none)
        
        Showmember -> 
            case model.clusterpost of
              Loaded hm->
                let
                  (sm, cmd) = Members.initialState hm.seqid
                in ( { model | memberpost = sm, ask=True }, Cmd.map MembersMsg cmd )
              _ -> 
                (model,Cmd.none)
        
        ShowQuality -> 
            ( {model | showQualityDetails = not model.showQualityDetails}, Cmd.none )

        CopyProtein ->
            ( { model | copiedField = Just ProteinSequence }, Cmd.none )

        CopyNucleotide ->
            ( { model | copiedField = Just NucleotideSequence }, Cmd.none )

        MembersMsg m -> 
            let
                (nqm, cmd) = Members.update m model.memberpost
            in
                ( { model | memberpost = nqm}, Cmd.map MembersMsg cmd )

        PopoverMsg1 state ->
            ( { model | popoverState1 = state }, Cmd.none )

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
        , div [ HtmlAttr.class "cluster-summary-grid" ]
            [ viewCard "Consensus sequences"
                [ viewAminoAcidField "Consensus protein sequence" v.aa (model.copiedField == Just ProteinSequence)
                , viewSequenceField "Consensus nucleotide sequence" "cluster-sequence" v.nuc CopyNucleotide (model.copiedField == Just NucleotideSequence)
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
        , p [ HtmlAttr.class "cluster-section-copy" ] [ text "Load the 100AA smORFs assigned to this 90AA cluster to inspect their sequences, habitats, and taxonomy." ]
        , if model.ask then
            Members.viewModel model.memberpost
                |> Html.map MembersMsg
          else
            div [ HtmlAttr.class "cluster-actions" ]
                [ Button.button [ Button.info, Button.onClick Showmember ] [ text "Load cluster members" ] ]
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
