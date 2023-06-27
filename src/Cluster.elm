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
import Json.Decode as D

import View exposing (View)
import Route exposing (Route)
import Members

type alias Cluster =
    { aa: String
    , habitat: String
    , nuc: String
    , seqid: String
    , tax: String
    }

type ClusterPost = 
    Loading
    | LoadError String
    | Loaded Cluster

type alias Model =
    { clusterpost : ClusterPost
    , memberpost : Members.Model
    , ask: Bool
    , navKey : Nav.Key
    }

type APIResult =
    APIError String
    | APIResultOK { aa: String
                  , habitat: String
                  , nuc: String
                  , seqid: String
                  , tax: String
                  }

type Msg
    = ResultsData ( Result Http.Error APIResult )
    | Showmember
    | MembersMsg Members.Msg

decodeAPIResult : D.Decoder APIResult
decodeAPIResult =
    let
        bAPIResultOK a h n s t = APIResultOK { aa = a, habitat = h, nuc = n, seqid = s, tax=t}
    in D.map5 bAPIResultOK
        (D.field "aminoacid" D.string)
        (D.field "habitat" D.string)
        (D.field "nucleotide" D.string)
        (D.field "seq_id" D.string)
        (D.field "taxonomy" D.string)


initialState : String -> Nav.Key -> (Model, Cmd Msg)
initialState seq_id navkey =
    ( { clusterpost = Loading
    , memberpost = Members.Loading 
    , ask = False
    , navKey = navkey
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
                in ( { model | memberpost = sm,ask=True}, Cmd.map MembersMsg cmd )
              _ -> 
                (model,Cmd.none)

        MembersMsg m -> 
            let
                (nqm, cmd) = Members.update m model.memberpost
            in
                ( { model | memberpost = nqm}, Cmd.map MembersMsg cmd )

viewModel : Model -> Html Msg
viewModel model =
    case (model.clusterpost,model.memberpost) of
        (Loading,_) ->
            div []
                [ text "Loading..."]
        (LoadError e,_) ->
            div []
                [ text "Error "
                , text e
                ]
        (Loaded v, Members.Loading) ->
            if model.ask == True then
                div [] 
                [ h1 [] [text v.seqid]
                , viewCluster v
                , title
                , Html.hr [] []
                , Members.viewModel model.memberpost
                    |> Html.map MembersMsg]
            else
                div [] 
                    [ h1 [] [text v.seqid]
                    , viewCluster v
                    , title
                    ]
        (Loaded v, Members.Results r) ->
            div [] 
                [ h1 [] [text v.seqid]
                , viewCluster v
                , title
                , Html.hr [] []
                , Members.viewModel model.memberpost
                    |> Html.map MembersMsg]
        (_,_) ->
            div []
                [ text "Loading..."]
                  

-- main text
title : Html Msg
title = div [ id "cluster" ] 
                [ h4 [id "cluster"] 
                       [ text  "This 90AA cluster contains the following 100AA smORFs:"]
                , Button.button [ Button.info, Button.onClick (Showmember)] [ text "Show" ]
                ]

viewCluster : Cluster -> Html Msg
viewCluster v = 
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
            , Table.tr []
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
                ]
            , Table.tr []
                [ Table.td [] [ p [id "title"] [text "Quality"]  ]
                , Table.td [] [ p [id "detail"] [text "-"]  ]
                ]
            ]
        }