module Test exposing (main)

import Html exposing (..)
import Html.Events exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid

import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser
import Dict
import Markdown
import View exposing (View)

import Bootstrap.Table as Table
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Json.Decode as D
import Json.Encode as Encode
import Dict exposing (Dict)
import Http

type alias SequenceResultFull =
    { aa: Maybe String
    , habitat: Maybe String
    , nuc: Maybe String
    , seqid: String
    , tax: Maybe String  }

type alias MultiResultItem =
    { aa: String
    , habitat: String
    , nuc: String
    , seqid: String
    , tax: String  }
    
type Model =
    Loading
    | LoadError String
    --| Results APIResult
    | MultiResults MultiResult

decodeSequenceResult : D.Decoder SequenceResultFull
decodeSequenceResult = 
    D.map5 SequenceResultFull
           (D.maybe (D.field "aminoacid" D.string))
           (D.maybe (D.field "habitat" D.string))
           (D.maybe (D.field "nucleotide" D.string))
           ((D.field "seq_id" D.string))
           (D.maybe (D.field "taxonomy" D.string))

decodeMultItemResult : D.Decoder MultiResultItem
decodeMultItemResult = 
    D.map5 MultiResultItem
           (D.field "aminoacid" D.string)
           (D.field "habitat" D.string)
           (D.field "nucleotide" D.string)
           (D.field "seq_id" D.string)
           (D.field "taxonomy" D.string)

type APIResult =
        APIResultOK { results : List SequenceResultFull
                    , status : String
                    }
        | APIError String

type MultiResult =
        MultiResultOK (List MultiResultItem)
        | MultiError String

type Msg
    --= ResultsData (Result Http.Error APIResult)
    = MultiData (Result Http.Error MultiResult)


decodeAPIResult : D.Decoder APIResult
decodeAPIResult =
    let
        bAPIResultOK r s = APIResultOK { results = r, status = s }
    in D.map2 bAPIResultOK
        (D.field "results" (D.list decodeSequenceResult))
        (D.field "status" D.string)

decodeMultiResult : D.Decoder MultiResult
decodeMultiResult =
    let
        bMultiResultOK r = MultiResultOK r
    in D.map bMultiResultOK
        (D.list decodeMultItemResult)

{-init : flags -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.post
    { url = "https://gmsc-api.big-data-biology.org/v1/seq-filter/"
    , body = Http.multipartBody
                [ Http.stringPart "habitat" "air"
                , Http.stringPart "taxonomy" "d__Bacteria"
                , Http.stringPart "hq_only" "True"
                ]
    , expect = Http.expectJson ResultsData decodeAPIResult
    }
    )-}

init : flags -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , Http.post
    { url = "https://gmsc-api.big-data-biology.org/v1/seq-info-multi/"
    --, body = Http.stringBody "application/json" """{"seq_ids":["GMSC10.90AA.123_456_789","GMSC10.90AA.123_456_790"]}"""
    , body = Http.jsonBody multi
    -- , body = Http.jsonBody multivalue
    , expect = Http.expectJson MultiData decodeMultiResult
    }
    )

{-
multi : Dict String (List String)
multi =
    Dict.fromList [ ("seq_ids",["GMSC10.90AA.123_456_789","GMSC10.90AA.123_456_790"]) ]

multivalue : Encode.Value
multivalue = 
    Encode.dict identity (Encode.list Encode.string) multi
-}

ids : List String
ids = 
  ["GMSC10.90AA.123_456_789","GMSC10.90AA.123_456_790"]
  
multi : Encode.Value
multi =
    Encode.object
        [  ("seq_ids", Encode.list Encode.string ids)
        ]

main : Program () Model Msg
main =
  Browser.document
    { init = init 
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        {-ResultsData r -> case r of
            Ok v -> ( Results v, Cmd.none )
            Err err -> case err of
                Http.BadUrl s -> (LoadError ("Bad URL: "++ s) , Cmd.none)
                Http.Timeout  -> (LoadError ("Timeout") , Cmd.none)
                Http.NetworkError -> (LoadError ("Network error!") , Cmd.none)
                Http.BadStatus s -> (LoadError (("Bad status: " ++ String.fromInt s)) , Cmd.none)
                Http.BadBody s -> (LoadError (("Bad body: " ++ s)) , Cmd.none)-}
        MultiData r -> case r of
            Ok v -> ( MultiResults v, Cmd.none )
            Err err -> case err of
                Http.BadUrl s -> (LoadError ("Bad URL: "++ s) , Cmd.none)
                Http.Timeout  -> (LoadError ("Timeout") , Cmd.none)
                Http.NetworkError -> (LoadError ("Network error!") , Cmd.none)
                Http.BadStatus s -> (LoadError (("Bad status: " ++ String.fromInt s)) , Cmd.none)
                Http.BadBody s -> (LoadError (("Bad body: " ++ s)) , Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

view : Model -> Browser.Document Msg
view model = { title = "GMSC"
        , body =
            [ CDN.stylesheet
            , CDN.fontAwesome
            , Html.node "link"
                [ HtmlAttr.rel "stylesheet"
                , HtmlAttr.href "/style.css"
                ]
                []
            , Grid.containerFluid []
                [ Grid.simpleRow
                    [ Grid.col []
                        [ viewModel model
                        ]
                    ]
                ]
            ]
        }

viewModel : Model-> Html Msg
viewModel model =
    case model of
        Loading ->
                div []
                    [ text "Loading..."
                    ]
        LoadError e ->
                div []
                    [ text "Error "
                    , text e
                    ]
        MultiResults r -> viewResults r

viewResults r  = case r of
    MultiResultOK ok ->
        div [id "member"]
            [ if List.isEmpty ok then
                    text "No small proteins in the selected habitats and/or taxonomy. Please try another selection."
              else Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "90AA accession" ]
                        , Table.th [] [ Html.text "Consensus protein sequence" ]
                        , Table.th [] [ Html.text "Consensus nucleotide sequence" ]
                        , Table.th [] [ Html.text "Habitat" ]
                        , Table.th [] [ Html.text "Taxonomy" ]
                        ]
                    , tbody = Table.tbody []
                            (List.map (\e ->
                                            Table.tr []
                                            [  Table.td [] [ p [id "identifier"] [Html.a [href ("/cluster/" ++ e.seqid)] [Html.text e.seqid] ] ]
                                            ,  Table.td [] [ p [id "detail"] [text e.aa ] ]
                                            ,  Table.td [] [ p [id "detail"] [text e.nuc ] ]
                                            ,  Table.td [] [ p [id "detail"] [text e.habitat ] ]
                                            ,  Table.td [] [ p [id "detail"] [text e.tax ] ]
                                            ]
                                    ) <|ok)
                    }
        ]
    MultiError err ->
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]

{-viewResults r  = case r of
    APIResultOK ok ->
        div [id "member"]
            [ if List.isEmpty ok.results then
                    text "No small proteins in the selected habitats and/or taxonomy. Please try another selection."
              else Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "90AA accession" ]
                        , Table.th [] [ Html.text "Consensus protein sequence" ]
                        , Table.th [] [ Html.text "Consensus nucleotide sequence" ]
                        , Table.th [] [ Html.text "Habitat" ]
                        , Table.th [] [ Html.text "Taxonomy" ]
                        ]
                    , tbody = Table.tbody []
                            (List.map (\e ->
                                case (e.aa, e.habitat) of 
                                  (Just a, Just h) ->
                                    case ( e.nuc, e.tax ) of 
                                        (Just n, Just t) ->
                                            Table.tr []
                                            [  Table.td [] [ p [id "identifier"] [Html.a [href ("/cluster/" ++ e.seqid)] [Html.text e.seqid] ] ]
                                            ,  Table.td [] [ p [id "detail"] [text a ] ]
                                            ,  Table.td [] [ p [id "detail"] [text n ] ]
                                            ,  Table.td [] [ p [id "detail"] [text h ] ]
                                            ,  Table.td [] [ p [id "detail"] [text t ] ]
                                            ]
                                        (_, _) ->
                                            Table.tr []
                                            [ Table.td [] [ p [id "identifier"] [Html.a [href ("/cluster/" ++ e.seqid)] [Html.text e.seqid] ] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ]
                                  (_, _) ->
                                    Table.tr []
                                      [  Table.td [] [ p [id "identifier"] [Html.a [href ("/cluster/" ++ e.seqid)] [Html.text e.seqid] ] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ]
                                    ) <|ok.results)
                    }
        ]
    APIError err ->
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]
-}