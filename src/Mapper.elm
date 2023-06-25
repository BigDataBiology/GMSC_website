module Mapper exposing (Model(..), Msg(..), initialState, update, viewModel)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser
import Dict
import Markdown
import Http

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Table as Table
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Json.Decode as D
import Delay

import View exposing (View)

type alias QueryResult =
  { seqid : String
  , aa : String
  , habitat: String
  , hits: List HitsResult
  , quality: String
  , tax: String
  }

type alias HitsResult = 
    { e: Float
    , id: String
    , identity: Float
    }

type alias SequenceResult =
    { aa: String
    , habitat: String
    , hits: List HitsResult
    , quality: String
    , tax: String
    }

type alias SearchResult =
    { results : Maybe (Dict.Dict String QueryResult)
    , search_id : String
    , status : String
   }
type SearchResultOrError =
    SearchResultOk SearchResult
    | SearchResultError String


decodeSearchResult : D.Decoder SearchResultOrError
decodeSearchResult =
    let
        bSearchResultOK r i s = SearchResultOk { results = r, search_id = i, status = s }
    in D.map3 bSearchResultOK
        (D.maybe (D.field "results" decodeQueryResult))
        (D.field "search_id" D.string)
        (D.field "status" D.string)

decodeQueryResult : D.Decoder (Dict.Dict String QueryResult)
decodeQueryResult = D.map (Dict.map seqToquery) (D.dict decodeSequenceResult)

seqToquery : String -> SequenceResult -> QueryResult
seqToquery seqid { aa, habitat, hits, quality, tax } =
  QueryResult seqid aa habitat hits quality tax

decodeSequenceResult : D.Decoder SequenceResult
decodeSequenceResult = 
    D.map5 SequenceResult
        (D.field "aminoacid" D.string)
        (D.field "habitat" D.string)
        (D.field "hits" (D.list decodeHitsResult))
        (D.field "quality" D.string)
        (D.field "taxonomy" D.string)

decodeHitsResult : D.Decoder HitsResult
decodeHitsResult =
    D.map3 HitsResult
        (D.field "evalue" D.float)
        (D.field "id" D.string)
        (D.field "identity" D.float)


type Model =
    Loading
    | LoadError String
    | SearchError String
    | Search SearchResult


type Msg
    = SearchData (Result Http.Error SearchResultOrError)
    | Getresults String

initialState : String -> (Model, Cmd Msg)
initialState seq =
    ( Loading
    , Http.post
    { url = "https://gmsc-api.big-data-biology.org/internal/seq-search/"
    , body = Http.multipartBody
                [ Http.stringPart "sequence_faa" seq
                ]
    , expect = Http.expectJson SearchData decodeSearchResult
    }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchData sd -> case sd of
            Ok (SearchResultOk v) ->
                (Search v,
                    if v.status == "Done"
                    then Cmd.none
                    else Delay.after 5000 (Getresults v.search_id))
            Ok (SearchResultError e) -> (SearchError e, Cmd.none)
            Err err -> case err of
                Http.BadUrl s -> (LoadError ("Bad URL: "++ s) , Cmd.none)
                Http.Timeout  -> (LoadError ("Timeout") , Cmd.none)
                Http.NetworkError -> (LoadError ("Network error!") , Cmd.none)
                Http.BadStatus s -> (LoadError (("Bad status: " ++ String.fromInt s)) , Cmd.none)
                Http.BadBody s -> (LoadError (("Bad body: " ++ s)) , Cmd.none)

        Getresults id ->
                ( Loading
                , Http.get { url = ("https://gmsc-api.big-data-biology.org/internal/seq-search/" ++ id)
                                   , expect = Http.expectJson SearchData decodeSearchResult
                           }
                )

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
        Search s -> viewSearch s
        SearchError err -> viewSearchError err

viewSearch : SearchResult -> Html Msg
viewSearch s  =
    if s.status == "Done" then
        case s.results of
          Just r ->
            div []
            [Table.table
                    { options = [ Table.striped, Table.hover ]
                    , thead =  Table.simpleThead
                        [ Table.th [] [ Html.text "Query sequence" ]
                        , Table.th [] [ Html.text "Protein sequence" ]
                        , Table.th [] [ Html.text "Habitat" ]
                        , Table.th [] [ Html.text "Taxonomy" ]
                        ]
                    , tbody = Table.tbody []
                    (Dict.toList r
                        |> List.map (\(k,v) ->
                            Table.tr []
                            [ Table.td [] [Html.text k]
                            , Table.td [] [Html.text v.aa]
                            , Table.td [] [Html.text v.habitat]
                            , Table.td [] [Html.text v.tax]
                            ]
                            )
                        )
                    }
            ]
          Nothing ->
            div [] [text s.search_id]

        else
            Html.div []
                [ Html.p []
                    [Html.text "Search results are still not available (it may take 10-15 minutes). "
                    ,Html.text "Current status is "
                    ,Html.strong [] [Html.text (if s.status == "Ok" then "Submitted" else s.status)]
                    ,Html.text "."
                    ]
                , Html.p []
                    [Html.text "The page will refresh automatically every 5 seconds..." ]
                ]

viewSearchError : String -> Html Msg
viewSearchError err =
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]
