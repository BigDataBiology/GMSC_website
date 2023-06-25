module Members exposing (APIResult(..), Model(..), Msg(..), initialState, update, viewModel)

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

import Http

type alias SequenceResultFull =
    { aa: Maybe String
    , habitat: Maybe String
    , nuc: Maybe String
    , seqid: String
    , tax: Maybe String  }

type Model =
    Loading
    | LoadError String
    | Results APIResult

decodeSequenceResult : D.Decoder SequenceResultFull
decodeSequenceResult = 
    D.map5 SequenceResultFull
           (D.maybe (D.field "aminoacid" D.string))
           (D.maybe (D.field "habitat" D.string))
           (D.maybe (D.field "nucleotide" D.string))
           ((D.field "seq_id" D.string))
           (D.maybe (D.field "taxonomy" D.string))

type APIResult =
        APIResultOK { cluster : List SequenceResultFull
                    , status : String
                    }
        | APIError String

type Msg
    = ResultsData (Result Http.Error APIResult)


decodeAPIResult : D.Decoder APIResult
decodeAPIResult =
    let
        bAPIResultOK r s = APIResultOK { cluster = r, status = s }
    in D.map2 bAPIResultOK
        (D.field "cluster" (D.list decodeSequenceResult))
        (D.field "status" D.string)

initialState : String -> (Model, Cmd Msg)
initialState seq_id = 
    ( Loading
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/cluster-info/" ++ seq_id)
    , expect = Http.expectJson ResultsData decodeAPIResult
    }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultsData r -> case r of
            Ok v -> ( Results v, Cmd.none )
            Err err -> case err of
                Http.BadUrl s -> (LoadError ("Bad URL: "++ s) , Cmd.none)
                Http.Timeout  -> (LoadError ("Timeout") , Cmd.none)
                Http.NetworkError -> (LoadError ("Network error!") , Cmd.none)
                Http.BadStatus s -> (LoadError (("Bad status: " ++ String.fromInt s)) , Cmd.none)
                Http.BadBody s -> (LoadError (("Bad body: " ++ s)) , Cmd.none)

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
        Results r -> viewResults r


viewResults r  = case r of
    APIResultOK ok -> 
        div [id "member"]
            [  Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "100AA accession" ]
                        , Table.th [] [ Html.text "Protein sequence" ]
                        , Table.th [] [ Html.text "Nucleotide sequence" ]
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
                                            [  Table.td [] [ p [id "detail"] [text e.seqid] ]
                                            ,  Table.td [] [ p [id "detail"] [text a ] ]
                                            ,  Table.td [] [ p [id "detail"] [text n ] ]
                                            ,  Table.td [] [ p [id "detail"] [text h ] ]
                                            ,  Table.td [] [ p [id "detail"] [text t ] ]
                                            ]
                                        (_, _) ->
                                            Table.tr []
                                            [ Table.td [] [ p [id "detail"] [text e.seqid] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                            ]
                                  (_, _) ->
                                    Table.tr []
                                      [  Table.td [] [ p [id "detail"] [text e.seqid] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ,  Table.td [] [ p [id "detail"] [text "-"] ]
                                      ]
                                    ) <|ok.cluster)
                    }
        ]
    APIError err ->
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]