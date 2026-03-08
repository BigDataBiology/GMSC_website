module Filter exposing (BrowsePost(..), BrowseResults, Model, Msg(..), initialState, update, viewModel)

import Html exposing (Html, div, p, text)
import Html.Attributes as HtmlAttr

import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Dropdown as Dropdown
import Json.Decode as D
import Http
import Status
import TaxonomyView
import Utils.ResultsPipeline as ResultsPipeline

type alias SequenceResultFull =
    { aa: Maybe String
    , habitat: Maybe String
    , nuc: Maybe String
    , seqid: String
    , tax: Maybe String  }

type alias Model =
    { browsepost : BrowsePost
    , results : ResultsPipeline.State
    }

type BrowsePost
    = BLoading
    | BLoadError String
    | Results BrowseResults

decodeSequenceResult : D.Decoder SequenceResultFull
decodeSequenceResult = 
    D.map5 SequenceResultFull
           (D.maybe (D.field "aminoacid" D.string))
           (D.maybe (D.field "habitat" D.string))
           (D.maybe (D.field "nucleotide" D.string))
           ((D.field "seq_id" D.string))
           (D.maybe (D.field "taxonomy" D.string))

type alias BrowseResults =
    { results : List SequenceResultFull
    , status : String
    }

type Msg
    = ResultsData (Result Http.Error BrowseResults)
    | DownloadResults
    | MultiData (Result Http.Error ResultsPipeline.MultiResult)
    | ShowPage Int
    | DropdownMsg Dropdown.State

decodeAPIResult : D.Decoder BrowseResults
decodeAPIResult =
    D.map2 BrowseResults
        (D.field "results" (D.list decodeSequenceResult))
        (D.field "status" D.string)

initialState : String -> String -> String -> String -> String -> String -> String -> String -> String -> (Model, Cmd Msg)
initialState habitat taxonomy antifam terminal rnacode metat riboseq metap hq=
    ( { browsepost = BLoading
      , results = ResultsPipeline.initialState
      }
    , Http.post
    { url = "https://gmsc-api.big-data-biology.org/v1/seq-filter/"
    , body = Http.multipartBody
                [ Http.stringPart "habitat" habitat
                , Http.stringPart "taxonomy" taxonomy
                , Http.stringPart "quality_antifam" antifam
                , Http.stringPart "quality_terminal" terminal
                , Http.stringPart "quality_rnacode" rnacode
                , Http.stringPart "quality_metat" metat
                , Http.stringPart "quality_riboseq" riboseq
                , Http.stringPart "quality_metap" metap
                , Http.stringPart "hq_only" hq
                ]
    , expect = Http.expectJson ResultsData decodeAPIResult
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
                        ResultsPipeline.pageIds page .seqid ok.results
                in
                ( { model | browsepost = Results ok, results = nextResults }
                , ResultsPipeline.fetchPage MultiData ids
                )
            Err err ->
                ( { model | browsepost = BLoadError (ResultsPipeline.httpErrorMessage err) }, Cmd.none )

        DownloadResults ->
            ( model, ResultsPipeline.downloadResults "result.tsv" model.results )

        MultiData r ->
            ( { model | results = ResultsPipeline.updateShowPost r model.results }, Cmd.none )

        ShowPage page ->
            case model.browsepost of
                Results ok ->
                    let
                        nextResults =
                            ResultsPipeline.setLoadingPage page model.results

                        ids =
                            ResultsPipeline.pageIds page .seqid ok.results
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
    case model.results.showpost of
        ResultsPipeline.SLoading ->
                Status.loading
                    "Loading browse results"
                    "Fetching the selected clusters and their annotations."
        ResultsPipeline.SLoadError e ->
                div []
                    [ text "Error "
                    , text e
                    ]
        ResultsPipeline.MultiResults r -> 
            case model.browsepost of 
                Results b ->
                    viewResults r b model
                _ ->
                    Status.loading
                        "Preparing browse results"
                        "The site is still collecting the selected cluster identifiers."


viewResults r b model = case r of
    ResultsPipeline.MultiResultOK ok ->
        div []
            [ div [ HtmlAttr.class "context-callout" ]
                [ p [ HtmlAttr.class "context-copy" ]
                    [ text "Browse results are 90AA family-level clusters. Each row links to a cluster page, where you can inspect the family summary and load the linked 100AA non-redundant members." ]
                ]
            , if List.length b.results /= 0 then
                div [HtmlAttr.class "action-row"] [Button.button [ Button.info, Button.onClick DownloadResults] [ Html.text "Download results" ] ]
              else div [] [text ""]
            , if List.isEmpty ok then
                    text "No small proteins in the selected habitats and/or taxonomy. Please try another selection."
              else div []
                [ div [HtmlAttr.class "results-wrap"]
                    [ Table.table
                        { options = [ Table.striped, Table.hover ]
                        , thead =  Table.simpleThead
                            [ Table.th [] [ Html.text "90AA accession" ]
                            , Table.th [] [ Html.text "Consensus protein sequence" ]
                            , Table.th [] [ Html.text "Consensus nucleotide sequence" ]
                            , Table.th [] [ Html.text "Habitat" ]
                            , Table.th [] [ Html.text "Taxonomy" ]
                            ]
                        , tbody = Table.tbody []
                                (List.map (\e ->
                                                Table.tr []
                                                [  Table.td [] [ p [HtmlAttr.class "table-identifier"] [Html.a [HtmlAttr.href ("/cluster/" ++ e.seqid)] [Html.text e.seqid] ] ]
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
                    , totalItems = List.length b.results
                    , onSelectPage = ShowPage
                    , dropdownMsg = DropdownMsg
                    }
                ]
            ]
    ResultsPipeline.MultiError err ->
        div []
            [ Html.p [] [ Html.text "Call to the GMSC server failed" ]
            , Html.blockquote []
                [ Html.p [] [ Html.text err ] ]
            ]
