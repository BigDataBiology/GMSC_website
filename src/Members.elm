module Members exposing (MemberPost(..), MemberResults, Model, Msg(..), initialState, loadingModel, update, viewModel)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Table as Table
import Chart as C
import Chart.Attributes as CA
import Dict
import File.Download as Download
import Html exposing (Html, div, h5, p, text)
import Html.Attributes as HtmlAttr
import Json.Decode as D
import Http
import Status
import TaxonomyView
import Utils.ResultsPipeline as ResultsPipeline


type SequenceResult
    = SequenceResultFull
        { seqid : String
        , aa : String
        , habitat : String
        , nuc : String
        , tax : String
        }
    | SequenceResultShallow { seqid : String }


type alias Model =
    { memberpost : MemberPost
    , detailStatus : DetailStatus
    , page : Int
    , dropdownState : Dropdown.State
    }


type MemberPost
    = MLoading
    | MLoadError String
    | Results MemberResults


type DetailStatus
    = DetailsLoading
    | DetailsLoadError String
    | DetailsReady


decodeSequenceResult : D.Decoder SequenceResult
decodeSequenceResult =
    D.oneOf
        [ D.map5 (\s a h n t -> SequenceResultFull { seqid = s, aa = a, habitat = h, nuc = n, tax = t })
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
    , detailStatus = DetailsLoading
    , page = 1
    , dropdownState = Dropdown.initialState
    }


initialState : String -> (Model, Cmd Msg)
initialState seq_id =
    ( loadingModel
    , Http.get
        { url = "https://gmsc-api.big-data-biology.org/v1/cluster-info/" ++ seq_id
        , expect = Http.expectJson ResultsData decodeClusterInfo
        }
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResultsData r ->
            case r of
                Ok ok ->
                    if pageIsDetailed 1 ok then
                        ( { model | memberpost = Results ok, detailStatus = DetailsReady, page = 1 }, Cmd.none )
                    else
                        let
                            page =
                                1

                            ids =
                                ResultsPipeline.pageIds page sequenceId ok.cluster
                        in
                        ( { model | memberpost = Results ok, detailStatus = DetailsLoading, page = page }
                        , ResultsPipeline.fetchPage MultiData ids
                        )

                Err err ->
                    ( { model | memberpost = MLoadError (ResultsPipeline.httpErrorMessage err) }, Cmd.none )

        DownloadResults ->
            case model.memberpost of
                Results memberResults ->
                    if pageIsDetailed model.page memberResults then
                        let
                            allresults =
                                ResultsPipeline.pageItems model.page memberResults.cluster
                                    |> List.filterMap detailedRow
                                    |> List.map (\seq -> String.join "\t" [ seq.seqid, seq.aa, seq.nuc, seq.habitat, seq.tax ])
                                    |> String.join "\n"
                        in
                        ( model, Download.string "cluster.members.tsv" "text/plain" allresults )
                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        MultiData r ->
            case ( model.memberpost, r ) of
                ( Results memberResults, Ok (ResultsPipeline.MultiResultOK rows) ) ->
                    ( { model
                        | memberpost = Results (mergeDetailedRows model.page rows memberResults)
                        , detailStatus = DetailsReady
                      }
                    , Cmd.none
                    )

                ( _, Err err ) ->
                    ( { model | detailStatus = DetailsLoadError (ResultsPipeline.httpErrorMessage err) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ShowPage page ->
            case model.memberpost of
                Results ok ->
                    if pageIsDetailed page ok then
                        ( { model | page = page, detailStatus = DetailsReady }, Cmd.none )
                    else
                        let
                            ids =
                                ResultsPipeline.pageIds page sequenceId ok.cluster
                        in
                        ( { model | page = page, detailStatus = DetailsLoading }
                        , ResultsPipeline.fetchPage MultiData ids
                        )

                _ ->
                    ( model, Cmd.none )

        DropdownMsg state ->
            ( { model | dropdownState = state }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Dropdown.subscriptions model.dropdownState DropdownMsg


viewModel : Model -> Html Msg
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
            viewResults m model


viewResults : MemberResults -> Model -> Html Msg
viewResults m model =
    Html.div []
        [ viewMembersSummary m
        , viewDetailStatus model.detailStatus
        , if pageIsDetailed model.page m then
            div [ HtmlAttr.class "action-row" ] [ Button.button [ Button.info, Button.onClick DownloadResults ] [ Html.text "Download members" ] ]
          else
            text ""
        , viewMembersTable m model
        , ResultsPipeline.viewPager
            { state = pagerState model
            , totalItems = List.length m.cluster
            , onSelectPage = ShowPage
            , dropdownMsg = DropdownMsg
            }
        ]


viewDetailStatus : DetailStatus -> Html Msg
viewDetailStatus detailStatus =
    case detailStatus of
        DetailsLoading ->
            Status.loading
                "Loading member annotations"
                "Detailed annotations for this page are still being fetched."

        DetailsLoadError errorMessage ->
            div []
                [ text "Error "
                , text errorMessage
                ]

        DetailsReady ->
            text ""


viewMembersSummary : MemberResults -> Html Msg
viewMembersSummary m =
    Html.div []
        [ viewSummary m
        , div [ HtmlAttr.class "context-callout" ]
            [ p [ HtmlAttr.class "context-copy" ]
                [ text "This member list shows the 100AA non-redundant smORF accessions assigned to the current 90AA family. Full rows display the individual annotations already available in GMSC." ]
            ]
        , Html.div []
            [ Html.p [ HtmlAttr.style "float" "left" ] [ Html.text ("Number of smORFs in cluster: " ++ String.fromInt (List.length m.cluster)) ]
            , Html.div []
                (if anyShallow m.cluster then
                    [ Html.p [ HtmlAttr.style "float" "left" ] [ Html.strong [] [ Html.text "Note: The cluster is too large. Not displaying the distribution of all sequences" ] ] ]
                 else
                    []
                )
            ]
        ]


viewMembersTable : MemberResults -> Model -> Html Msg
viewMembersTable m model =
    div [ HtmlAttr.class "results-wrap" ]
        [ Table.table
            { options = [ Table.striped, Table.hover ]
            , thead =
                Table.simpleThead
                    [ Table.th [] [ Html.text "100AA accession" ]
                    , Table.th [] [ Html.text "Protein sequence" ]
                    , Table.th [] [ Html.text "Nucleotide sequence" ]
                    , Table.th [] [ Html.text "Habitat" ]
                    , Table.th [] [ Html.text "Taxonomy" ]
                    ]
            , tbody =
                Table.tbody []
                    (ResultsPipeline.pageItems model.page m.cluster
                        |> List.map viewMemberRow
                    )
            }
        ]


mergeDetailedRows : Int -> List ResultsPipeline.MultiResultItem -> MemberResults -> MemberResults
mergeDetailedRows page rows memberResults =
    let
        startIndex =
            (page - 1) * 100

        endIndex =
            startIndex + 100

        rowById =
            rows
                |> List.map (\row -> ( row.seqid, row ))
                |> Dict.fromList

        mergeEntry index entry =
            if index < startIndex || index >= endIndex then
                entry
            else
                case Dict.get (sequenceId entry) rowById of
                    Just row ->
                        SequenceResultFull
                            { seqid = row.seqid
                            , aa = row.aa
                            , habitat = row.habitat
                            , nuc = row.nuc
                            , tax = row.tax
                            }

                    Nothing ->
                        entry
    in
    { memberResults | cluster = List.indexedMap mergeEntry memberResults.cluster }


pagerState : Model -> ResultsPipeline.State
pagerState model =
    { showpost = ResultsPipeline.SLoading
    , page = model.page
    , dropdownState = model.dropdownState
    }


viewMemberRow : SequenceResult -> Table.Row msg
viewMemberRow entry =
    case entry of
        SequenceResultFull full ->
            Table.tr []
                [ Table.td [] [ p [ HtmlAttr.class "table-identifier" ] [ Html.a [ HtmlAttr.href ("/sequence/" ++ full.seqid) ] [ Html.text full.seqid ] ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail" ] [ text full.aa ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail" ] [ text full.nuc ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail" ] [ text full.habitat ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail" ] [ TaxonomyView.view full.tax ] ]
                ]

        SequenceResultShallow shallow ->
            Table.tr []
                [ Table.td [] [ p [ HtmlAttr.class "table-identifier" ] [ Html.a [ HtmlAttr.href ("/sequence/" ++ shallow.seqid) ] [ Html.text shallow.seqid ] ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail table-detail-pending" ] [ text "Pending" ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail table-detail-pending" ] [ text "Pending" ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail table-detail-pending" ] [ text "Pending" ] ]
                , Table.td [] [ p [ HtmlAttr.class "table-detail table-detail-pending" ] [ text "Pending" ] ]
                ]


detailedRow : SequenceResult -> Maybe { seqid : String, aa : String, nuc : String, habitat : String, tax : String }
detailedRow entry =
    case entry of
        SequenceResultFull full ->
            Just full

        SequenceResultShallow _ ->
            Nothing


pageIsDetailed : Int -> MemberResults -> Bool
pageIsDetailed page memberResults =
    ResultsPipeline.pageItems page memberResults.cluster
        |> List.all
            (\entry ->
                case entry of
                    SequenceResultFull _ ->
                        True

                    SequenceResultShallow _ ->
                        False
            )


anyShallow : List SequenceResult -> Bool
anyShallow =
    List.any
        (\e ->
            case e of
                SequenceResultShallow _ ->
                    True

                SequenceResultFull _ ->
                    False
        )


viewSummary ok =
    let
        datahabitat =
            summaryForhabitat ok.cluster

        datatax =
            summaryFortax ok.cluster
    in
    Html.div []
        [ Html.div
            [ HtmlAttr.style "width" "460px"
            , HtmlAttr.style "margin-left" "4em"
            , HtmlAttr.style "float" "left"
            ]
            [ Html.h5 [] [ Html.text "Habitat distribution" ]
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
                    [ C.bar .count [ CA.color CA.green ] ]
                    datahabitat
                ]
            ]
        , Html.div
            [ HtmlAttr.style "width" "460px"
            , HtmlAttr.style "margin-left" "4em"
            , HtmlAttr.style "float" "left"
            ]
            [ Html.h5 [] [ Html.text "Taxonomy distribution" ]
            , C.chart
                [ CA.height 190
                , CA.width 460
                , CA.margin { top = 10, bottom = 40, left = 20, right = 20 }
                , CA.padding { top = 10, bottom = 10, left = 10, right = 10 }
                ]
                [ C.binLabels .tax [ CA.moveDown 50, CA.fontSize 12, CA.rotate 20 ]
                , C.yLabels [ CA.withGrid, CA.fontSize 12 ]
                , C.bars []
                    [ C.bar .count [ CA.color CA.blue ] ]
                    datatax
                ]
            ]
        ]


summaryForhabitat : List SequenceResult -> List { habitat : String, count : Float }
summaryForhabitat seqs =
    let
        add1 : Maybe Float -> Maybe Float
        add1 c =
            case c of
                Nothing ->
                    Just 1.0

                Just x ->
                    Just (x + 1.0)
    in
    List.foldl
        (\e acc ->
            case e of
                SequenceResultShallow _ ->
                    acc

                SequenceResultFull f ->
                    Dict.update f.habitat add1 acc
        )
        Dict.empty
        seqs
        |> Dict.toList
        |> List.map (\( habitat, count ) -> { habitat = habitat, count = count })


summaryFortax : List SequenceResult -> List { tax : String, count : Float }
summaryFortax seqs =
    let
        add1 : Maybe Float -> Maybe Float
        add1 c =
            case c of
                Nothing ->
                    Just 1.0

                Just x ->
                    Just (x + 1.0)
    in
    List.foldl
        (\e acc ->
            case e of
                SequenceResultShallow _ ->
                    acc

                SequenceResultFull f ->
                    let
                        taxlist =
                            List.reverse (String.split ";" f.tax)

                        species =
                            if List.length taxlist == 7 then
                                String.join "" (List.take 1 taxlist)
                            else
                                "Unknown"
                    in
                    Dict.update species add1 acc
        )
        Dict.empty
        seqs
        |> Dict.toList
        |> List.map (\( tax, count ) -> { tax = tax, count = count })
