module Members exposing (MemberPost(..), MemberResults, Model, Msg(..), initialState, loadingModel, update, viewModel)

import Html exposing (Html, div, h5, p, text)
import Html.Events exposing (onClick)

import Chart as C
import Chart.Attributes as CA

import Html.Attributes as HtmlAttr
import Dict

import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Json.Decode as D
import Http
import Status
import TaxonomyView
import Utils.ResultsPipeline as ResultsPipeline

type SequenceResult =
    SequenceResultFull
        { seqid: String
        , aa: String
        , habitat: String
        , nuc: String
        , tax: String  }
    | SequenceResultShallow { seqid: String }

type alias Model =
    { memberpost : MemberPost
    , results : ResultsPipeline.State
    }

type MemberPost
    = MLoading
    | MLoadError String
    | Results MemberResults

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

type alias MemberResults =
    { cluster : List SequenceResult
    , status : String
    }

type Msg
    = ResultsData (Result Http.Error MemberResults)
    | DownloadResults
    | MultiData (Result Http.Error ResultsPipeline.MultiResult)
    | ShowPage Int
    | DropdownMsg Dropdown.State

decodeClusterInfo : D.Decoder MemberResults
decodeClusterInfo =
    D.map2 MemberResults
        (D.field "cluster" (D.list decodeSequenceResult))
        (D.field "status" D.string)

sequenceId : SequenceResult -> String
sequenceId seq =
    case seq of
        SequenceResultFull full ->
            full.seqid

        SequenceResultShallow shallow ->
            shallow.seqid


loadingModel : Model
loadingModel =
    { memberpost = MLoading
    , results = ResultsPipeline.initialState
    }

initialState : String -> (Model, Cmd Msg)
initialState seq_id =
    ( loadingModel
    , Http.get
    { url = ("https://gmsc-api.big-data-biology.org/v1/cluster-info/" ++ seq_id)
    , expect = Http.expectJson ResultsData decodeClusterInfo
    }
    )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultsData r -> case r of
            Ok ok ->
                let
                    page =
                        1

                    nextResults =
                        ResultsPipeline.setLoadingPage page model.results

                    ids =
                        ResultsPipeline.pageIds page sequenceId ok.cluster
                in
                ( { model | memberpost = Results ok, results = nextResults }
                , ResultsPipeline.fetchPage MultiData ids
                )
            Err err ->
                ( { model | memberpost = MLoadError (ResultsPipeline.httpErrorMessage err) }, Cmd.none )

        DownloadResults ->
            ( model, ResultsPipeline.downloadResults "cluster.members.tsv" model.results )

        MultiData r ->
            ( { model | results = ResultsPipeline.updateShowPost r model.results }, Cmd.none )

        ShowPage page ->
            case model.memberpost of
                Results ok ->
                    let
                        nextResults =
                            ResultsPipeline.setLoadingPage page model.results

                        ids =
                            ResultsPipeline.pageIds page sequenceId ok.cluster
                    in
                    ( { model | results = nextResults }
                    , ResultsPipeline.fetchPage MultiData ids
                    )

                _ ->
                    ( model, Cmd.none )

        DropdownMsg state ->
            ( { model | results = ResultsPipeline.updateDropdownState state model.results }, Cmd.none )

subscriptions : Model -> Sub Msg
subscriptions model =
    ResultsPipeline.subscriptions model.results DropdownMsg

viewModel : Model-> Html Msg
viewModel model =
    case model.memberpost of
        MLoading ->
            Status.loading
                "Loading cluster members"
                "Fetching the 100AA smORFs assigned to this 90AA cluster."

        MLoadError e ->
            div []
                [ text "Error "
                , text e
                ]

        Results m ->
            case model.results.showpost of
                ResultsPipeline.SLoading ->
                    viewPartialResults m model

                ResultsPipeline.SLoadError e ->
                    viewPartialResultsWithError m model e

                ResultsPipeline.MultiResults r ->
                    viewResults r m model


viewPartialResults : MemberResults -> Model -> Html Msg
viewPartialResults m model =
    Html.div []
        [ viewMembersSummary m
        , Status.loading
            "Loading member annotations"
            "Detailed annotations for this page are still being fetched."
        , viewPartialMembersTable m model
        , ResultsPipeline.viewPager
            { state = model.results
            , totalItems = List.length m.cluster
            , onSelectPage = ShowPage
            , dropdownMsg = DropdownMsg
            }
        ]


viewPartialResultsWithError : MemberResults -> Model -> String -> Html Msg
viewPartialResultsWithError m model errorMessage =
    Html.div []
        [ viewMembersSummary m
        , div []
            [ text "Error "
            , text errorMessage
            ]
        , viewPartialMembersTable m model
        , ResultsPipeline.viewPager
            { state = model.results
            , totalItems = List.length m.cluster
            , onSelectPage = ShowPage
            , dropdownMsg = DropdownMsg
            }
        ]

viewResults r m model = case r of
    ResultsPipeline.MultiResultOK ok ->
        Html.div []
            [ viewMembersSummary m
            , div [HtmlAttr.class "action-row"] [Button.button [ Button.info, Button.onClick DownloadResults] [ Html.text "Download members" ]]
            , div [HtmlAttr.class "results-wrap"]
              [ Table.table
                { options = [ Table.striped, Table.hover ]
                , thead =  Table.simpleThead
                    [ Table.th [] [ Html.text "100AA accession" ]
                    , Table.th [] [ Html.text "Protein sequence" ]
                    , Table.th [] [ Html.text "Nucleotide sequence" ]
                    , Table.th [] [ Html.text "Habitat" ]
                    , Table.th [] [ Html.text "Taxonomy" ]
                    ]
                , tbody = Table.tbody []
                        ( List.map (\e ->
                                        Table.tr []
                                        [  Table.td [] [ p [HtmlAttr.class "table-identifier"] [Html.a [HtmlAttr.href ("/sequence/" ++ e.seqid)] [Html.text e.seqid] ] ]
                                        ,  Table.td [] [ p [HtmlAttr.class "table-detail"] [text e.aa ] ]
                                        ,  Table.td [] [ p [HtmlAttr.class "table-detail"] [text e.nuc ] ]
                                        ,  Table.td [] [ p [HtmlAttr.class "table-detail"] [text e.habitat ] ]
                                        ,  Table.td [] [ p [HtmlAttr.class "table-detail"] [TaxonomyView.view e.tax ] ]
                                        ]
                                   ) ok
                        )
                }
              ]
            , ResultsPipeline.viewPager
                { state = model.results
                , totalItems = List.length m.cluster
                , onSelectPage = ShowPage
                , dropdownMsg = DropdownMsg
                }
            ]
    ResultsPipeline.MultiError err ->
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]


viewMembersSummary : MemberResults -> Html Msg
viewMembersSummary m =
    Html.div []
        [ viewSummary m
        , Html.div []
            [ Html.p [HtmlAttr.style "float" "left"] [ Html.text ("Number of smORFs in cluster: " ++ String.fromInt (List.length m.cluster) )]
            , Html.div []
                (if anyShallow m.cluster then
                    [ Html.p [HtmlAttr.style "float" "left"] [ Html.strong [] [Html.text "Note: The cluster is too large. Not displaying the distribution of all sequences"] ] ]
                 else
                    []
                )
            ]
        ]


viewPartialMembersTable : MemberResults -> Model -> Html Msg
viewPartialMembersTable m model =
    div [HtmlAttr.class "results-wrap"]
        [ Table.table
            { options = [ Table.striped, Table.hover ]
            , thead = Table.simpleThead
                [ Table.th [] [ Html.text "100AA accession" ]
                , Table.th [] [ Html.text "Protein sequence" ]
                , Table.th [] [ Html.text "Nucleotide sequence" ]
                , Table.th [] [ Html.text "Habitat" ]
                , Table.th [] [ Html.text "Taxonomy" ]
                ]
            , tbody = Table.tbody []
                (ResultsPipeline.pageItems model.results.page m.cluster
                    |> List.map
                        (\entry ->
                            case entry of
                                SequenceResultFull full ->
                                    Table.tr []
                                        [ Table.td [] [ p [HtmlAttr.class "table-identifier"] [Html.a [HtmlAttr.href ("/sequence/" ++ full.seqid)] [Html.text full.seqid] ] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail"] [text full.aa ] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail"] [text full.nuc ] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail"] [text full.habitat ] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail"] [TaxonomyView.view full.tax ] ]
                                        ]

                                SequenceResultShallow shallow ->
                                    Table.tr []
                                        [ Table.td [] [ p [HtmlAttr.class "table-identifier"] [Html.a [HtmlAttr.href ("/sequence/" ++ shallow.seqid)] [Html.text shallow.seqid] ] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail table-detail-pending"] [text "Pending"] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail table-detail-pending"] [text "Pending"] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail table-detail-pending"] [text "Pending"] ]
                                        , Table.td [] [ p [HtmlAttr.class "table-detail table-detail-pending"] [text "Pending"] ]
                                        ]
                        )
                )
            }
        ]


anyShallow : List SequenceResult -> Bool
anyShallow =
    List.any (\e -> case e of
        SequenceResultShallow _ -> True
        SequenceResultFull _ -> False
        )

viewSummary ok =
    let
        datahabitat = summaryForhabitat ok.cluster
        datatax = summaryFortax ok.cluster
    in Html.div []
        [ Html.div
          [ HtmlAttr.style "width" "460px"
          , HtmlAttr.style "margin-left" "4em"
          , HtmlAttr.style "float" "left"
          ]
            [ Html.h5 [] [Html.text "Habitat distribution"]
            , C.chart
              [ CA.height 190
              , CA.width 460
              , CA.margin { top = 10, bottom = 40, left = 20, right = 20 }
              , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
              ]
              [ C.grid []
              , C.binLabels .habitat [ CA.moveDown 20, CA.fontSize 12 ]
              , C.yLabels [ CA.withGrid, CA.fontSize 12 ]
              , C.bars []
                [ C.bar .count [CA.color CA.green]
                ]
                datahabitat
              ]
            ]
        , Html.div
          [ HtmlAttr.style "width" "460px"
          , HtmlAttr.style "margin-left" "4em"
          , HtmlAttr.style "float" "left"
          ]
            [ Html.h5 [] [Html.text "Taxonomy distribution"]
            , C.chart
              [ CA.height 190
              , CA.width 460
              , CA.margin { top = 10, bottom = 40, left = 20, right = 20 }
              , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
              ]
              [ C.binLabels .tax [ CA.moveDown 50, CA.fontSize 12, CA.rotate 20 ]
              , C.yLabels [ CA.withGrid, CA.fontSize 12 ]
              , C.bars []
                [ C.bar .count [CA.color CA.blue]
                ]
                datatax
              ]
            ]
        ]


summaryForhabitat : List SequenceResult -> List ({ habitat : String, count : Float })
summaryForhabitat seqs =
    let
        add1 : Maybe Float -> Maybe Float
        add1 c = case c of
            Nothing -> Just 1.0
            Just x -> Just (x + 1.0)
    in
        List.foldl (\e acc -> case e of
                                SequenceResultShallow _ -> acc
                                SequenceResultFull f -> Dict.update f.habitat add1 acc) Dict.empty seqs
        |> Dict.toList
        |> List.map (\(habitat, count) -> { habitat = habitat, count = count })

summaryFortax : List SequenceResult -> List ({ tax : String, count : Float })
summaryFortax seqs =
    let
        add1 : Maybe Float -> Maybe Float
        add1 c = case c of
            Nothing -> Just 1.0
            Just x -> Just (x + 1.0)
    in
        List.foldl (\e acc -> case e of
                                SequenceResultShallow _ -> acc
                                SequenceResultFull f ->
                                    let taxlist = List.reverse (String.split ";" f.tax)
                                        species = if List.length taxlist == 7 then
                                                    String.join "" (List.take 1 taxlist)
                                                  else
                                                    "Unknown"
                                    in Dict.update species add1 acc) Dict.empty seqs
        |> Dict.toList
        |> List.map (\(tax, count) -> { tax = tax, count = count })
