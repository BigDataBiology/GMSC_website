module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (..)

type Route
    = NotFound
    | HomeR
    | SequenceR String
    | ClusterR String
    --| MapperR 
    | BrowseR 
    | DownloadR 
    | HelpR
    | AboutR

parseUrl : Url -> Route
parseUrl url =
    case parse matchRoute url of
        Just route ->
            route

        Nothing ->
            HomeR

matchRoute : Parser (Route -> a) a
matchRoute =
    oneOf
        [ Parser.map HomeR Parser.top
        , Parser.map HomeR (Parser.s "home")
        , Parser.map BrowseR (Parser.s "browse")
        , Parser.map DownloadR (Parser.s "downloads")
        , Parser.map HelpR (Parser.s "help")
        , Parser.map AboutR (Parser.s "about")
        , Parser.map SequenceR (Parser.s "sequence" </> Parser.string)
        , Parser.map ClusterR (Parser.s "cluster" </> Parser.string)
        ]

pushUrl : Route -> Nav.Key -> Cmd msg
pushUrl route navKey =
    routeToString route
        |> Nav.pushUrl navKey

routeToString : Route -> String
routeToString route =
    case route of
        NotFound ->
            "/not-found"

        HomeR ->
            "/home"

        BrowseR ->
            "/browse"

        DownloadR ->
            "/downloads"

        HelpR ->
            "/help"

        AboutR ->
            "/about"

        SequenceR seq_id ->
            "/sequence/" ++ seq_id
        
        ClusterR seq_id ->
            "/cluster/" ++ seq_id