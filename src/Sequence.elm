module Sequence exposing (Model, Msg(..), initialState, update, viewModel)

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
import Bootstrap.Button as Button
import Bootstrap.Table as Table
import Json.Decode as D
import RemoteData exposing (WebData)

import View exposing (View)
import Route exposing (Route)

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
      }
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/seq-info/" ++ seq_id)
       , expect =  postDecoder
                |> Http.expectJson (RemoteData.fromResult >> PostReceived)
    }
    )

type Msg
    = PostReceived (WebData Post)

update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        PostReceived post ->
            ( { model | post = post }, Cmd.none )

viewModel : Model-> Html.Html Msg
viewModel model =
    case model.post of
        RemoteData.NotAsked ->
            text ""
        RemoteData.Loading ->
            div [] [ text "Loading Post..." ]
        RemoteData.Success v ->
            div []
                    [ h1 [] [text v.seqid]
                    , Table.table { options = [ Table.striped, Table.small,Table.hover]
                                  , thead = Table.simpleThead []
                                  , tbody = Table.tbody []
                                      [ Table.tr []
                                          [ Table.td [] [p [id "title"] [text "Protein sequence"]  ]
                                          , Table.td [] [p [id "detail"] [text v.aa] ]
                                          ]
                                      , Table.tr []
                                          [ Table.td [] [ p [id "title"] [text "Nucleotide sequence"] ]
                                          , Table.td [] [ p [id "detail"] [text v.nuc] ]
                                          ]
                                      , Table.tr []
                                          [ Table.td [] [ p [id "title"] [text "Taxonomic assignment"]  ]
                                          , Table.td [] [ p [id "detail"] [text v.tax]  ]
                                          ]
                                      , Table.tr []
                                          [ Table.td [] [ p [id "title"] [text "Habitat"]  ]
                                          , Table.td [] [ p [id "detail"] [text v.habitat]  ]
                                          ]
                                      , Table.tr []
                                          [ Table.td [] [ p [id "title"] [text "Protein cluster"]  ]
                                          , Table.td [] [a [ href "https://guide.elm-lang.org" ] [ text "-" ]]
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
                                          [ Table.td [] [ p [id "title"] [text "Quality"]  ]
                                          , Table.td [] [ p [id "detail"] [text "-"]  ]
                                          ]
                                     ]
                                  }
                    ]
        RemoteData.Failure httpError ->
            viewFetchError (buildErrorMessage httpError)

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message
            
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