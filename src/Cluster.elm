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

ifHighQuality : Bool -> Bool -> Float -> Int -> Int -> Float -> String
ifHighQuality a t r mt rb mp = 
    if a == True && t == True && r < 0.05 && (mt > 1 || rb > 1 || mp > 0.5) then
      "High quality"
    else
      "Not high quality"

ifRNAcode : Float -> String
ifRNAcode n = 
  if n == 2 then 
    "Not perform"
  else 
    if n < 0.05 then 
      "Pass (" ++ (String.fromFloat n) ++ ")"
    else 
      "Not Pass (" ++ (String.fromFloat n) ++ ")"

stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "Pass"
  else
    "Not pass"

ifmetaTR : Int -> String
ifmetaTR n = 
  if n > 1 then
    "Pass (" ++ (String.fromInt n) ++ ")"
  else
    "Not Pass (" ++ (String.fromInt n) ++ ")"

ifmetaP : Float -> String
ifmetaP n = 
  if n > 0.5 then
    "Pass (" ++ (String.fromFloat n) ++ ")"
  else
    "Not Pass (" ++ (String.fromFloat n) ++ ")"
=======
stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "True"
  else
    "False"
>>>>>>> 1e355cea14ac9d33b70bec9f7f1d483a6db93136

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
    , ifquality : Bool
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
    | Showquality
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
    , ifquality = False
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
        
        Showquality -> 
            ( {model | ifquality = True}, Cmd.none )

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
                , viewCluster v model.ifquality model.popoverState1
                , title
                , Html.hr [] []
                , Members.viewModel model.memberpost
                    |> Html.map MembersMsg]
            else
                div [] 
                    [ h1 [] [text v.seqid]
                    , viewCluster v model.ifquality model.popoverState1
                    , title
                    ]
        (Loaded v, Members.MultiResults r) ->
            div [] 
                [ h1 [] [text v.seqid]
                , viewCluster v model.ifquality model.popoverState1
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
                [ Table.td [] [ p [id "title"] [text "Number of 100AA smORFs"]  ]
                , Table.td [] [ p [id "detail"] [text "-"]  ]
                ]-}
            , Table.tr []
<<<<<<< HEAD
                [ Table.td [] [ p [id "title"] [ text "Quality  "
                                               , Popover.config
                                                 ( Button.button
                                                   [ Button.small
                                                   , Button.outlineInfo
                                                   , Button.attrs <|
                                                       Popover.onHover pop PopoverMsg1
                                                   ]
                                                   [ span [class "fa fa-question-circle"]
                                                   []
                                                   ]
                                                 )
                                                 |> Popover.right
                                                 |> Popover.content []
                                                      [ text "High quality is defined as: Pass all in silico quality tests (Not in Antifam database, Pass the terminal checking, P-value of RNAcode < 0.05) and contained at least one experimental evidence (Align to at least 2 metatranscriptomic samples, alignment to at least 2 Riboseq samples, or the coverage of metaproteomic peptides on small protein sequences >0.5)" ]
                                                 |> Popover.view pop
                                               ]   
                              ]
                , Table.td [] [ p [id "detail"] [text (ifHighQuality v.quality.antifam v.quality.terminal v.quality.rnacode v.quality.metat v.quality.riboseq v.quality.metap)]
                              , div [class "browse"] [ Button.button [ Button.info, Button.onClick (Showquality) ] [ text "Show detailed quality" ] ]
                              , if ifq == True then
                                  viewQuality v
                                else 
                                  p [] [text ""]
                              ]
=======
                [ Table.td [] [ p [id "title"] [text "Quality"]  ]
                , Table.td [] [ p [id "detail"] [text ("Antifam:" ++ (stringFromBool v.quality.antifam))]
                              , p [id "detail"] [text ("Terminal checking:" ++ (stringFromBool v.quality.terminal))]
                              , p [id "detail"] [text ("RNAcode:" ++ (String.fromFloat v.quality.rnacode))]
                              , p [id "detail"] [text ("metaTranscriptome:" ++ (String.fromInt v.quality.metat))] 
                              , p [id "detail"] [text ("Riboseq:" ++ (String.fromInt v.quality.riboseq))] 
                              , p [id "detail"] [text ("metaProteome: " ++ (String.fromFloat v.quality.metap))]
                                ]
>>>>>>> 1e355cea14ac9d33b70bec9f7f1d483a6db93136
                ]
            ]
        }

viewQuality : Cluster -> Html Msg
viewQuality v =  
  div [] 
      [ p [id "detail"] [text ("Antifam: " ++ (stringFromBool v.quality.antifam))]
      , p [id "detail"] [text ("Terminal checking: " ++ (stringFromBool v.quality.terminal))]
      , p [id "detail"] [text ("RNAcode: " ++ (ifRNAcode v.quality.rnacode))]
      , p [id "detail"] [text ("metaTranscriptome: " ++ (ifmetaTR v.quality.metat))] 
      , p [id "detail"] [text ("Riboseq: " ++ (ifmetaTR v.quality.riboseq))] 
      , p [id "detail"] [text ("metaProteome: " ++ (ifmetaP v.quality.metap))]
      ]