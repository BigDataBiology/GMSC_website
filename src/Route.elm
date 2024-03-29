module Route exposing (Route(..), parseUrl, pushUrl)

import Browser.Navigation as Nav
import Url exposing (Url)
import Url.Parser as Parser exposing (..)

type Route
    = NotFound
    | HomeR
    | SequenceR String
    | ClusterR String
    | MapperR
    | MapperResultR String
    | BrowseR 
    | DownloadR 
    | HelpR
    | AboutR
    | IndexR

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
        , Parser.map IndexR (Parser.s "index.bs")
        , Parser.map SequenceR (Parser.s "sequence" </> Parser.string)
        , Parser.map ClusterR (Parser.s "cluster" </> Parser.string)
        , Parser.map MapperR (Parser.s "mapper")
        , Parser.map MapperResultR (Parser.s "mapper" </> Parser.string)
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

        IndexR ->
            "/index.bs"

        SequenceR seq_id ->
            "/sequence/" ++ seq_id
        
        ClusterR seq_id ->
            "/cluster/" ++ seq_id

        MapperR ->
            "/mapper"

        MapperResultR search_id->
            "/mapper/" ++ search_id