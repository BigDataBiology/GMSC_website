module Members exposing (APIResult(..), Model(..), Msg(..), initialState, update, viewModel)

import Html exposing (..)
import Html.Events exposing (..)
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
import File.Download as Download
import Http

type SequenceResult =
    SequenceResultFull
        { seqid: String
        , aa: String
        , habitat: String
        , nuc: String
        , tax: String  }
    | SequenceResultShallow { seqid: String }

type Model =
    Loading
    | LoadError String
    | Results APIResult

decodeSequenceResult : D.Decoder SequenceResult
decodeSequenceResult =
    D.oneOf
        [D.map5 (\s a h n t -> SequenceResultFull { seqid = s, aa = a, habitat = h, nuc = n, tax = t })
           (D.field "seq_id" D.string)
           (D.field "aminoacid" D.string)
           (D.field "habitat" D.string)
           (D.field "nucleotide" D.string)
           (D.field "taxonomy" D.string)
        , D.map (\s -> SequenceResultShallow { seqid = s })
            (D.field "seq_id" D.string)
        ]


type APIResult =
        APIResultOK { cluster : List SequenceResult
                    , status : String
                    }
        | APIError String

type Msg
    = ResultsData (Result Http.Error APIResult)
    | DownloadResults

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

        DownloadResults -> case model of
            Results (APIResultOK v) ->
                let allresults =
                        v.cluster
                        |> List.map (\seq -> case seq of
                                        SequenceResultShallow _ -> ""
                                        SequenceResultFull f -> String.join "\t" [f.seqid, f.aa, f.nuc, f.habitat, f.tax])
                        |> String.join "\n"
                in ( model, Download.string "cluster.members.tsv" "text/plain" allresults)
            _ -> ( model, Cmd.none )

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
        Results (APIError err) ->
                div []
                    [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
                    , Html.blockquote []
                        [ Html.p [] [ Html.text err ] ]
                    ]
        Results (APIResultOK ok) -> viewResults ok


viewResults ok  =
    div [id "member"]
          [ Button.button [ Button.info, Button.onClick DownloadResults, Button.attrs [ class "float-right"]] [ Html.text "Download members" ]
          , Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =  Table.simpleThead
                    [ Table.th [] [ Html.text "100AA accession" ]
                    , Table.th [] [ Html.text "Protein sequence" ]
                    , Table.th [] [ Html.text "Nucleotide sequence" ]
                    , Table.th [] [ Html.text "Habitat" ]
                    , Table.th [] [ Html.text "Taxonomy" ]
                    ]
                , tbody = Table.tbody []
                        <| (ok.cluster
                            |>  List.map (\e -> case e of
                                SequenceResultShallow s ->
                                    Table.tr []
                                        [  Table.td [] [ p [id "identifier"] [Html.a [href ("/sequence/" ++ s.seqid)] [Html.text s.seqid] ] ]
                                        ,  Table.td [] [ p [id "detail"] [text ""] ]
                                        ,  Table.td [] [ p [id "detail"] [text ""] ]
                                        ,  Table.td [] [ p [id "detail"] [text ""] ]
                                        ,  Table.td [] [ p [id "detail"] [text ""] ]
                                        ]
                                SequenceResultFull f ->
                                    Table.tr []
                                        [  Table.td [] [ p [id "identifier"] [Html.a [href ("/sequence/" ++ f.seqid)] [Html.text f.seqid] ] ]
                                        ,  Table.td [] [ p [id "detail"] [text f.aa ] ]
                                        ,  Table.td [] [ p [id "detail"] [text f.nuc ] ]
                                        ,  Table.td [] [ p [id "detail"] [text f.habitat ] ]
                                        ,  Table.td [] [ p [id "detail"] [text f.tax ] ]
                                        ]
                                )
                            )
                }
    ]

