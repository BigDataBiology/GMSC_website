module Cluster exposing (Model, Msg(..), initialState, update, viewModel)

import Html
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser.Navigation as Nav
import Browser
import Dict
import Markdown
import Http

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Popover as Popover
import Json.Decode as D

import View exposing (View)
import Route exposing (Route)
import Members


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
    | Showmember
    | ShowQuality
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
                   , times = 1
                   , myDrop1State = Dropdown.initialState
                   }
    , ask = False
    , showQualityDetails = False
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
            Ok (APIResultOK v) -> ( { model | clusterpost = Loaded v }, Cmd.none )
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
            ( {model | showQualityDetails = True}, Cmd.none )

        MembersMsg m -> 
            let
                (nqm, cmd) = Members.update m model.memberpost
            in
                ( { model | memberpost = nqm}, Cmd.map MembersMsg cmd )

        PopoverMsg1 state ->
            ( { model | popoverState1 = state }, Cmd.none )

viewModel : Model -> Html Msg
viewModel model =
    case (model.clusterpost,model.memberpost.showpost) of
        (Loading,_) ->
            div []
                [ text "Loading..."]
        (LoadError e,_) ->
            div []
                [ text "Error "
                , text e
                ]
        (Loaded v, Members.SLoading) ->
            if model.ask == True then
                div []
                [ h1 [] [text v.seqid]
                , viewCluster v model.showQualityDetails model.popoverState1
                , title
                , Html.hr [] []
                , Members.viewModel model.memberpost
                    |> Html.map MembersMsg]
            else
                div [] 
                    [ h1 [] [text v.seqid]
                    , viewCluster v model.showQualityDetails model.popoverState1
                    , title
                    ]
        (Loaded v, Members.MultiResults r) ->
            div [] 
                [ h1 [] [text v.seqid]
                , viewCluster v model.showQualityDetails model.popoverState1
                , title
                , Html.hr [] []
                , Members.viewModel model.memberpost
                    |> Html.map MembersMsg]
        (_,_) ->
            div []
                [ text "Loading..."]
                  

-- main text
title : Html Msg
title = div [] 
            [ h4 [id "cluster"] [ text  "This 90AA cluster contains the following 100AA smORFs:" ]
            , div [class "browse"] [ Button.button [ Button.info, Button.onClick (Showmember) ] [ text "Show" ] ]
            ]

viewCluster : Cluster -> Bool -> Popover.State -> Html Msg
viewCluster v ifq pop = 
   Table.table 
        { options = [ Table.striped, Table.small,Table.hover]
        , thead =  Table.simpleThead []
        , tbody = Table.tbody []
            [ Table.tr []
                [ Table.td [] [p [id "title"] [text "Consensus protein sequence"]  ]
                , Table.td [] [p [id "detail"] [text v.aa] ]
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Consensus nucleotide sequence"] ]
                , Table.td [] [ p [id "detail"] [text v.nuc] ]
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Taxonomic assignment"] ]
                , Table.td [] [ p [id "detail"] [text v.tax] ]
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Habitat"]  ]
                , Table.td [] [ p [id "detail"] [text v.habitat]  ]
                ]
            {-, Table.tr []
                [ Table.td [] [ p [id "title"] [text "Conserved domain"]  ]
                , Table.td [] [ p [id "detail"] [text "-"]  ]
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Cellular localization"]  ]
                , Table.td [] [ p [id "detail"] [text "-"]  ]
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Number of 100AA smORFs"] ]
                , Table.td [] [ p [id "detail"] [text "-"] ]
                ]-}
            , Table.tr []
                [ Table.td [] [ p [id "title"] [ text "Quality  "
                                               , Popover.config
                                                 ( Button.button
                                                   [ Button.small
                                                   , Button.outlineInfo
                                                   , Button.attrs <|
                                                       Popover.onHover pop PopoverMsg1
                                                   ]
                                                   [ span [ class "fa fa-question-circle" ] [] ]
                                                 )
                                                 |> Popover.right
                                                 |> Popover.content []
                                                      [ text "High quality is defined as: Pass all in silico quality tests (Not in Antifam database, Pass the terminal checking, P-value of RNAcode < 0.05) and contained at least one item of experimental evidence (Align to at least 2 metatranscriptomic samples, alignment to at least 2 Riboseq samples, or the coverage of metaproteomic peptides on small protein sequences >0.5)" ]
                                                 |> Popover.view pop
                                               ]   
                              ]
                , Table.td [] [ p [id "detail"] [text (qualityString v.quality)]
                              , if ifq then
                                  viewQualityDetails v
                                else
                                  div [class "browse"] [ Button.button [ Button.info, Button.onClick ShowQuality ] [ text "Show detailed quality information" ] ]
                              ]
                ]
            ]
        }

viewQualityDetails : Cluster -> Html Msg
viewQualityDetails v =  
  div []
      [ p [class "detail"] [text ("Antifam: " ++ (passOrFail v.quality.antifam))]
      , p [class "detail"] [text ("Terminal checking: " ++ (passOrFail v.quality.terminal))]
      , p [class "detail"] [text ("RNAcode: " ++ (rnaCodeString v.quality.rnacode))]
      , p [class "detail"] [text ("metaTranscriptome: " ++ (metaTRString v.quality.metat))]
      , p [class "detail"] [text ("Riboseq: " ++ (metaTRString v.quality.riboseq))]
      , p [class "detail"] [text ("metaProteome: " ++ (metaPString v.quality.metap))]
      ]

