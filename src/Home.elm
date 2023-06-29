module Home exposing (Model, Msg(..), initialModel, update, viewModel)
import Html exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Browser
import Dict
import Markdown
import View exposing (View)

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
import Bootstrap.Popover as Popover
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Carousel as Carousel exposing (defaultStateOptions)
import Bootstrap.Card as Card
import Bootstrap.Text as Text
import Bootstrap.Card.Block as Block
import Bootstrap.Alert as Alert


type OperationType = Contigs | Proteins

type alias Model =
    { optype : OperationType
    , is_contigs : String
    , idcontent : String
    , seqcontent: String
    , lookupIDContent : String
    , carouselState : Carousel.State
    , popoverState : Popover.State
    }


type Msg
    = SelectOp OperationType
    | SetIdentifierExample
    | SetIdentifier String
    | SetSequence String
    | SetLookupId String
    | SetSeqExample
    | ClearId
    | ClearSeq
    | CarouselMsg Carousel.Msg
    | PopoverMsg Popover.State
    | SubmitIdentifier
    | SubmitSequence
    | LookupSearch

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
            Carousel.subscriptions model.carouselState CarouselMsg

myOptions =
    { defaultStateOptions
        | interval = Nothing
        , pauseOnHover = False
    }

initialModel :  Model
initialModel =
        { optype = Proteins
        , is_contigs = "False"
        , idcontent = ""
        , seqcontent = ""
        , lookupIDContent = ""
        , carouselState = Carousel.initialStateWithOptions myOptions
        , popoverState = Popover.initialState
        }

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg)
update msg qmodel =
    case msg of
        SelectOp p ->
                -- If the example input is selected, switch it
                if qmodel.optype == Contigs && qmodel.seqcontent == contigExample && p == Proteins then
                    ( { qmodel | optype = Proteins, seqcontent = "" }, Cmd.none )
                else if qmodel.optype == Proteins && qmodel.seqcontent == proteinExample && p == Contigs then
                    ( { qmodel | optype = Contigs, is_contigs = "True", seqcontent = "" }, Cmd.none )
                else
                    ( { qmodel | optype = p, seqcontent = ""}, Cmd.none )

        SetIdentifierExample ->
            ( { qmodel | idcontent = identifierExample }, Cmd.none )

        SetIdentifier id ->
            ( { qmodel | idcontent = id }, Cmd.none )

        SetSequence s ->
            ( { qmodel | seqcontent = s }, Cmd.none )

        SetLookupId s ->
            ( { qmodel | lookupIDContent = s }, Cmd.none )

        SetSeqExample ->
            let
              nc =
                case qmodel.optype of
                  Contigs -> contigExample
                  Proteins -> proteinExample
            in
              ( { qmodel | seqcontent = nc }, Cmd.none )

        ClearId ->
            ( { qmodel | idcontent = "" }, Cmd.none )

        ClearSeq ->
            ( { qmodel | seqcontent = "" }, Cmd.none )

        CarouselMsg subMsg ->
            ({ qmodel | carouselState = Carousel.update subMsg qmodel.carouselState }, Cmd.none)

        PopoverMsg state ->
            ( { qmodel | popoverState = state }, Cmd.none )

        SubmitIdentifier -> (qmodel, Cmd.none)
        SubmitSequence -> (qmodel, Cmd.none)
        LookupSearch -> (qmodel, Cmd.none)

viewModel : Model -> Html Msg
viewModel model =
    Html.div []
        [ intro
        , viewSearch model
        , viewFig model
        ]

-- main text


intro : Html msg
intro =
    span [id "introduction"]
        [Markdown.toHtml [] """
# Global Microbial smORFs Catalog v1.0

The global microbial smORF catalogue (GMSC) is an integrated, consistently-processed, smORFs catalogue of the microbial world, combining publicly available metagenomes and high-quality isolated microbial genomes.
A total of non-redundant ~965 million 100AA ORFs were predicted from 63,410 metagenomes from global habitats and 87,920 high-quality isolated microbial genomes from the [ProGenomes](https://progenomes.embl.de/) database.
The smORFs were clustered at 90% amino acid identity resulting in ~288 million 90AA smORFs families.

- The annotation of GMSC contains:
  - taxonomy classification
  - habitat assignment
  - quality assessment
  - conserved domain annotation
  - cellular localization prediction
""" ]

viewSearch : Model -> Html Msg
viewSearch model =
  let
    buttonStyle who active =
                [ if who == active then Button.info else Button.outlineInfo, Button.onClick (SelectOp who) ]
    in div [class "search"] 
        [ Form.form []
            [ Form.row []
                [ Form.col [ Col.sm10 ] 
                    [ h4 [] [ text "Search from identifier or find homologues by sequence (GMSC-mapper)" ] ]
                ]
            , Form.row []
                [ Form.col [ Col.sm10 ]
                    [ Form.group []
                        [ Form.label [id "browse"] [ text "Search from Identifier" ]
                        , Input.text
                            [ Input.value model.idcontent
                            , Input.attrs
                                [ placeholder "GMSC10.100AA.XXX_XXX_XXX   or   GMSC10.90AA.XXX_XXX_XXX" ]
                            , Input.onInput SetIdentifier
                            ]
                        , Button.button [ Button.info, Button.attrs [ class "float-right"], Button.onClick SubmitIdentifier ] [ text "Submit" ] 
                        , Button.button[ Button.light, Button.attrs [ class "float-right"], Button.onClick ClearId ] [ text "Clear" ]
                        , Button.button [ Button.outlineSecondary, Button.attrs [ class "float-right"], Button.onClick SetIdentifierExample ] [ text "Example" ] 
                        ]
                    ]
                ]
            , Form.row []
                [ Form.col [ Col.sm10 ]
                    [ ButtonGroup.buttonGroup [ ButtonGroup.small ]
                        [ ButtonGroup.button (buttonStyle Proteins model.optype) [ text "Search from proteins" ]
                        , ButtonGroup.button (buttonStyle Contigs model.optype) [ text "Search from contigs" ]
                        ]
                    ]
                ]
            ]
        , Form.form []
            [ Form.row []
                [ Form.col [ Col.sm10 ]
                    [ Form.group []
                        [ label [ id "browse"] [ text "Input an amino acid / nucleotide sequence in FASTA format"]
                        , Textarea.textarea
                            [ Textarea.id "myarea"
                            , Textarea.value model.seqcontent
                            , Textarea.onInput SetSequence
                            , Textarea.rows 3
                            , Textarea.attrs [
                                placeholder <| if model.optype == Contigs then
                                    ">contigID\n AATACTACATGTCA..."
                                else
                                    ">proteinID\n MTIISR..."]
                            ]
                        , Button.button [ Button.info, Button.attrs [ class "float-right"], Button.onClick SubmitSequence] [ text "Submit" ]
                        , Button.button [ Button.light,Button.attrs [ class "float-right"], Button.onClick ClearSeq] [ text "Clear" ]
                        , Button.button [ Button.outlineSecondary,Button.attrs [ class "float-right"], Button.onClick SetSeqExample] [ text "Example" ]
                        ]
                    ]
                ]
            ]
        , Form.form []
            [ Form.row []
                [ Form.col [ Col.sm10 ]
                    [ Form.group []
                        [ Form.label [id "browse"] [ text "Lookup a sequence search result" ]
                        , Input.text
                            [ Input.value model.lookupIDContent
                            , Input.attrs
                                [ placeholder "1-xxxx" ]
                            , Input.onInput SetLookupId
                            ]
                        , Button.button
                            [ Button.info
                            , Button.attrs [ class "float-right"]
                            , Button.onClick LookupSearch
                            ]
                            [ text "Lookup" ]
                        ]
                    ]
                ]
            , Form.row []
                [ Form.col [ Col.sm10 ]
                    [ Alert.simpleInfo [] [ p [] [ text "This webserver allows you to use GMSC-mapper for short jobs. For larger jobs, you can download and use the "
                                                 , a [href "https://github.com/BigDataBiology/GMSC-mapper"] [text "command line version of the tool."]
                                                 ]
                                          , p [] [text "The searching will take a few minutes. To lookup the sequence search results, please type your search ID above that automatically generated when submitting."]
                                          ] 
                    ]
                ]
            ]
        ]

viewFig: Model -> Html Msg
viewFig model =
  div [class "fig"]
    [ Carousel.config CarouselMsg []
        |> Carousel.withControls
        |> Carousel.withIndicators
        |> Carousel.slides
            [Slide.config []
              (Slide.customContent
                (
                Card.config
                    [ Card.light
                    , Card.attrs [ ]
                    , Card.align Text.alignSmCenter
                    ]
                    |> Card.headerH4 []
                        [ img [ src "assets/home_geo.svg" ] []
                        , p [] [text " Geographical distribution"]
                        ]
                    |> Card.view
                )
              )
            , Slide.config []
              (Slide.customContent
                (
                Card.config
                    [ Card.light
                    , Card.attrs [ ]
                    , Card.align Text.alignSmCenter
                    ]
                    |> Card.headerH4 []
                        [ img [ src "assets/home_taxonomy.svg" ] []
                        , p [] [text " Taxonomy distribution"]
                        ]
                    |> Card.view
                )
              )
            , Slide.config []
              (Slide.customContent
                (
                Card.config
                    [ Card.light
                    , Card.attrs [ ]
                    , Card.align Text.alignSmCenter
                    ]
                    |> Card.headerH4 []
                        [ img [ src "assets/home_habitat_single.png" ] []
                        , p [] [text " Habitat distribution"]
                        ]
                    |> Card.view
                )
              )
            ]
        |> Carousel.view model.carouselState
    ]
identifierExample : String
identifierExample = "GMSC10.90AA.283_000_000"

contigExample : String
contigExample = """>scaffold1
CTTCTGATCTTTACGCAGCATTGTGTGTTTCCACCTTTCAAAAAATTCTCCGTGAACTGCGCCCTGGGAGTGGTGAAATCCTCCGCGGAACGAAGTCCCGGAATTGCGCACAAATTCACGTGCTGAACAATTTTACCATAGGAATGTGCGGTTGTAAAGAGAAAAATGCAAAAAATTCCTTATTTTTATAAAAGGAGCGGGGAAAAGAGGCGGAAAATATTTTTTTGAAAGGGGATTGACAGAGAGAAACGGCCGTGTTATCCTAACTGTACTAACACACATAGTACAGTTGGTACAGTTCGGAGGAACGTTATGAAGGTCATCAAGAAGGTAGTAGCCGCCCTGATGGTGCTGGGAGCACTGGCGGCGCTGACGGTAGGCGTGGTTTTGAAGCCGGGCCGGAAAGGAGACGAAACATGATGCTGTTTGGTTTTGCGGGGATCGCCGCCATCGTGGGTCTGATTTTTGCCGCTGTTGTTCTGGTGTCCGTGGCCTTGCAGCCCTGAGAACGGGGCAGATGCAATGAGTACGCTGTTTTTGCTTGGTATCGCAGGCGCGGTACTGCTGGTTATTTTGCTGACAGCGGTGATCCTGCACCGCTGATCGAACATTCCTCAGAAAGGAGAGGCACACGTTCTGACATTGAATTACCGGGATTCCCGTCCCATTTATGAACAGATCAAGGACGGCCTGCGGCGGATGATCGTCACCGGGGCC"""

proteinExample : String
proteinExample = """>smORF1
MTIISRNLFHFETVSRSLGIETEMNPFLGKHLNHQLIWCGLHGFGTAQAQAIGTEMQTNFRLFFAEFFPRFQDKPGARPLRRINPQGNLHIRFRC"""
