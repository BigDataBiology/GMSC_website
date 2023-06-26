module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing(..)
import Html.Attributes as HtmlAttr
import Html.Events exposing (..)
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, (</>), custom, fragment, map, oneOf, top)
import Dict
import Markdown
import View exposing (View)
import Route exposing (Route)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Carousel as Carousel
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Carousel as Carousel exposing (defaultStateOptions)
import Bootstrap.Card as Card
import Bootstrap.Text as Text
import Bootstrap.Card.Block as Block

import Home
import Sequence
import Cluster
import Mapper
import Browse
import Download
import Help
import About


type alias Model =
  { key : Nav.Key
  , page : Page
  , route : Route
  }

type Page =
    Home Home.Model
    | Sequence Sequence.Model
    | Cluster Cluster.Model
    | Mapper Mapper.Model
    | Browse Browse.Model
    | Download 
    | Help
    | About
    | NotFoundP

type Msg
    = HomeMsg Home.Msg
    | SequenceMsg Sequence.Msg
    | ClusterMsg Cluster.Msg
    | MapperMsg Mapper.Msg
    | BrowseMsg Browse.Msg
    | GoToHome
    | GoToBrowse
    | GoToDownload
    | GoToHelp
    | GoToAbout
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.none

-- INIT

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let model =
          { key = key
          , page = Home Home.initialModel
          , route = Route.parseUrl url
          }
    in 
    initCurrentPage ( model, Cmd.none )

initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundP, Cmd.none )

                Route.HomeR ->
                    ( Home Home.initialModel, Cmd.none )

                Route.BrowseR ->
                    ( Browse Browse.initialModel, Cmd.none )

                Route.DownloadR ->
                    ( Download, Cmd.none )

                Route.HelpR ->
                    ( Help, Cmd.none )

                Route.AboutR ->
                    ( About, Cmd.none )

                Route.SequenceR seq_id ->
                    let
                        ( pageModel, pageCmds ) =
                            Sequence.initialState seq_id model.key
                    in
                    ( Sequence pageModel, Cmd.map SequenceMsg pageCmds )
           
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = case msg of
    HomeMsg Home.SubmitIdentifier -> case model.page of
        Home hm ->
            if String.startsWith "GMSC10.100AA" hm.idcontent
              then
                let
                  (sm, cmd) = Sequence.initialState hm.idcontent model.key
                in ( { model | page = Sequence sm }
                   , Cmd.batch
                        [ Nav.pushUrl model.key ("/sequence/" ++ hm.idcontent)
                        , Cmd.map SequenceMsg cmd
                        ])
            else
                let
                  (sm, cmd) = Cluster.initialState hm.idcontent
                in ( { model | page = Cluster sm }
                   , Cmd.batch
                        [ Nav.pushUrl model.key ("/sequence/" ++ hm.idcontent)
                        , Cmd.map ClusterMsg cmd
                        ])
        _ -> ( model, Cmd.none )

    HomeMsg Home.SubmitSequence -> case model.page of
        Home hm ->
            let
                (mm, cmd) = Mapper.initialState hm.seqcontent
            in ( { model | page = Mapper mm } , Cmd.map MapperMsg cmd )
        _ -> ( model, Cmd.none )

    HomeMsg Home.LookupSearch -> case model.page of
        Home hm ->
            let
                (mm, cmd) = Mapper.lookupState hm.lookupIDContent
            in ( { model | page = Mapper mm } , Cmd.map MapperMsg cmd )
        _ -> ( model, Cmd.none )

    GoToHome ->
        ({ model | page = Home Home.initialModel },Cmd.none)

    GoToBrowse ->
        ({ model | page = Browse Browse.initialModel },Cmd.none)
    
    GoToDownload ->
        ({ model | page = Download },Cmd.none)

    GoToHelp ->
        ({ model | page = Help },Cmd.none)

    GoToAbout ->
        ({ model | page = About },Cmd.none)

    HomeMsg m -> case model.page of
        Home hm ->
            let
                (nhm, cmd) = Home.update m hm
            in
                ( { model | page = Home nhm }, Cmd.map HomeMsg cmd )
        _ -> ( model, Cmd.none )

    SequenceMsg m -> case model.page of
        Sequence sm ->
            let
                (nqm, cmd) = Sequence.update m sm
            in
                ( { model | page = Sequence nqm }, Cmd.map SequenceMsg cmd )
        _ -> ( model, Cmd.none )

    ClusterMsg m -> case model.page of
        Cluster sm ->
            let
                (nqm, cmd) = Cluster.update m sm
            in
                ( { model | page = Cluster nqm } , Cmd.map ClusterMsg cmd )
        _ -> ( model, Cmd.none )

    MapperMsg m -> case model.page of
        Mapper sm ->
            let
                (nqm, cmd) = Mapper.update m sm
            in
                ( { model | page = Mapper nqm } , Cmd.map MapperMsg cmd )
        _ -> ( model, Cmd.none )

    BrowseMsg m -> case model.page of
        Browse bm ->
            let
                (nbm, cmd) = Browse.update m bm
            in
                ( { model | page = Browse nbm } , Cmd.map BrowseMsg cmd )
        _ -> ( model, Cmd.none )  

    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model
          , Nav.pushUrl model.key (Url.toString url)
          )
        Browser.External href ->
          ( model
          , Nav.load href
          )

    UrlChanged url ->
      let
        newRoute =
            Route.parseUrl url
      in
        ( { model | route = newRoute }, Cmd.none )
            |> initCurrentPage

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }

view : Model -> Browser.Document Msg
view model = { title = "GMSC"
        , body =
            [ CDN.stylesheet
            , CDN.fontAwesome
            , Html.node "link"
                [ HtmlAttr.rel "stylesheet"
                , HtmlAttr.href "style.css"
                ]
                []
            , Grid.containerFluid []
                [ Grid.simpleRow
                    [ Grid.col []
                        [ header
                        , viewModel model
                        , Html.hr [] []
                        , footer
                        ]
                    ]
                ]
            ]
        }

viewModel : Model -> Html Msg
viewModel model = case model.page of
    Home m ->
        Home.viewModel m
            |> Html.map HomeMsg
    Sequence m ->
        Sequence.viewModel m
            |> Html.map SequenceMsg
    Cluster m ->
        Cluster.viewModel m
            |> Html.map ClusterMsg
    Mapper m ->
        Mapper.viewModel m
            |> Html.map MapperMsg        
    Browse m ->
        Browse.viewModel m
            |> Html.map BrowseMsg     
    Download ->
        Download.viewModel 
    Help ->
        Help.viewModel 
    About ->
        About.viewModel
    NotFoundP ->
        notFoundView

notFoundView : Html msg
notFoundView =
    h3 [] [ text "Oops! The page you requested was not found!" ]


-- header


header : Html Msg
header =
    Html.nav [id "topbar"]
        [Grid.simpleRow
            [ Grid.col [] [ Html.a [href "/home", onClick GoToHome] [Html.text "Home"]] 
            , Grid.col [] [ Html.a [href "/browse", onClick GoToBrowse] [Html.text "Browse"]] 
            , Grid.col [] [ Html.a [href "/downloads", onClick GoToDownload] [Html.text "Downloads"]]
            , Grid.col [] [ Html.a [href "/help", onClick GoToHelp] [Html.text "Help"]]
            , Grid.col [] [ Html.a [href "/about", onClick GoToAbout] [Html.text "About&Contact"]]
            ]
        ]

-- FOOTER


footer : Html Msg
footer =
  div [id "footerbar"]
      [  p [] [text "Copyright (c) 2023 GMSC authors. All rights reserved."]
      ]

