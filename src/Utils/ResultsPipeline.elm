module Utils.ResultsPipeline exposing
    ( MultiResult(..)
    , MultiResultItem
    , ShowPost(..)
    , State
    , decodeMultiResult
    , downloadResults
    , fetchPage
    , httpErrorMessage
    , initialState
    , pageCount
    , pageIds
    , pageItems
    , setLoadingPage
    , subscriptions
    , updateDropdownState
    , updateShowPost
    , viewPager
    )

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Utilities.Spacing as Spacing
import File.Download as Download
import Html exposing (Html, div, p, text)
import Html.Attributes as HtmlAttr
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Encode as Encode


pageSize : Int
pageSize =
    100


type alias MultiResultItem =
    { aa : String
    , habitat : String
    , nuc : String
    , seqid : String
    , tax : String
    }


type MultiResult
    = MultiResultOK (List MultiResultItem)
    | MultiError String


type ShowPost
    = SLoading
    | SLoadError String
    | MultiResults MultiResult


type alias State =
    { showpost : ShowPost
    , page : Int
    , dropdownState : Dropdown.State
    }


decodeMultiResultItem : D.Decoder MultiResultItem
decodeMultiResultItem =
    D.map5 MultiResultItem
        (D.field "aminoacid" D.string)
        (D.field "habitat" D.string)
        (D.field "nucleotide" D.string)
        (D.field "seq_id" D.string)
        (D.field "taxonomy" D.string)


decodeMultiResult : D.Decoder MultiResult
decodeMultiResult =
    D.map MultiResultOK (D.list decodeMultiResultItem)


initialState : State
initialState =
    { showpost = SLoading
    , page = 1
    , dropdownState = Dropdown.initialState
    }


httpErrorMessage : Http.Error -> String
httpErrorMessage err =
    case err of
        Http.BadUrl url ->
            "Bad URL: " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error!"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody body ->
            "Bad body: " ++ body


updateShowPost : Result Http.Error MultiResult -> State -> State
updateShowPost result state =
    case result of
        Ok multiResult ->
            { state | showpost = MultiResults multiResult }

        Err err ->
            { state | showpost = SLoadError (httpErrorMessage err) }


setLoadingPage : Int -> State -> State
setLoadingPage page state =
    { state | showpost = SLoading, page = page }


updateDropdownState : Dropdown.State -> State -> State
updateDropdownState dropdownState state =
    { state | dropdownState = dropdownState }


subscriptions : State -> (Dropdown.State -> msg) -> Sub msg
subscriptions state toMsg =
    Dropdown.subscriptions state.dropdownState toMsg


multi : List String -> Encode.Value
multi ids =
    Encode.object
        [ ( "seq_ids", Encode.list Encode.string ids ) ]


fetchPage : (Result Http.Error MultiResult -> msg) -> List String -> Cmd msg
fetchPage toMsg ids =
    Http.post
        { url = "https://gmsc-api.big-data-biology.org/v1/seq-info-multi/"
        , body = Http.jsonBody (multi ids)
        , expect = Http.expectJson toMsg decodeMultiResult
        }


pageItems : Int -> List a -> List a
pageItems page items =
    items
        |> List.drop (pageSize * (page - 1))
        |> List.take pageSize


pageIds : Int -> (a -> String) -> List a -> List String
pageIds page toId items =
    pageItems page items |> List.map toId


pageCount : Int -> Int
pageCount totalItems =
    if totalItems <= 0 then
        1
    else if modBy pageSize totalItems == 0 then
        totalItems // pageSize
    else
        (totalItems // pageSize) + 1


downloadResults : String -> State -> Cmd msg
downloadResults filename state =
    case state.showpost of
        MultiResults (MultiResultOK rows) ->
            rows
                |> List.map (\seq -> String.join "\t" [ seq.seqid, seq.aa, seq.nuc, seq.habitat, seq.tax ])
                |> String.join "\n"
                |> Download.string filename "text/plain"

        _ ->
            Cmd.none


displayRangeText : Int -> Int -> String
displayRangeText totalItems page =
    let
        startItem =
            ((page - 1) * pageSize) + 1

        endItem =
            min totalItems (page * pageSize)
    in
    "Displaying "
        ++ String.fromInt startItem
        ++ " to "
        ++ String.fromInt endItem
        ++ " of "
        ++ String.fromInt totalItems
        ++ " items."


viewPager :
    { state : State
    , totalItems : Int
    , onSelectPage : Int -> msg
    , dropdownMsg : Dropdown.State -> msg
    }
    -> Html msg
viewPager config =
    let
        currentPage =
            min config.state.page (pageCount config.totalItems)

        totalPages =
            pageCount config.totalItems

        pageButton label maybePage extraAttrs =
            case maybePage of
                Just page ->
                    Button.button
                        ([ Button.small
                         , Button.outlineInfo
                         , Button.attrs [ Spacing.ml1 ]
                         , Button.onClick (config.onSelectPage page)
                         ]
                            ++ extraAttrs
                        )
                        [ text label ]

                Nothing ->
                    Button.button
                        ([ Button.small
                         , Button.outlineInfo
                         , Button.attrs [ Spacing.ml1 ]
                         ]
                            ++ extraAttrs
                        )
                        [ text label ]

        dropdownItems =
            List.range 1 totalPages
                |> List.map
                    (\page ->
                        Dropdown.buttonItem
                            [ onClick (config.onSelectPage page) ]
                            [ text (String.fromInt page) ]
                    )
    in
    div [ HtmlAttr.class "browse" ]
        [ if config.totalItems > 0 then
            div [] [ p [] [ text (displayRangeText config.totalItems currentPage) ] ]
          else
            text ""
        , pageButton
            "<<"
            (if config.totalItems > pageSize then Just 1 else Nothing)
            [ Button.attrs [ HtmlAttr.class "float-left" ] ]
        , pageButton
            "<"
            (if currentPage > 1 then Just (currentPage - 1) else Nothing)
            [ Button.attrs [ HtmlAttr.class "float-left" ] ]
        , div [ HtmlAttr.style "float" "left" ]
            [ Dropdown.dropdown
                config.state.dropdownState
                { options = []
                , toggleMsg = config.dropdownMsg
                , toggleButton =
                    Dropdown.toggle
                        [ Button.small, Button.outlineInfo, Button.attrs [ HtmlAttr.class "float-left" ] ]
                        [ text "Page" ]
                , items =
                    if config.totalItems > pageSize then
                        dropdownItems
                    else
                        [ Dropdown.buttonItem [] [ text "1" ] ]
                }
            ]
        , pageButton
            ">"
            (if currentPage < totalPages then Just (currentPage + 1) else Nothing)
            []
        , pageButton
            ">>"
            (if config.totalItems > pageSize then Just totalPages else Nothing)
            []
        ]
