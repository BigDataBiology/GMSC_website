module Browse exposing (Model, Msg(..), initialModel, update, viewModel)

import Html exposing (..)
import Html.Attributes as HtmlAttr
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Browser
import Dict
import Set
import Markdown
import View exposing (View)

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form as Form
import Bootstrap.Form.Select as Select
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Textarea as Textarea
import Bootstrap.Dropdown as Dropdown
import Bootstrap.Card as Card
import Bootstrap.Text as Text
import Bootstrap.Card.Block as Block
import Bootstrap.Popover as Popover
import Bootstrap.Form.Checkbox as Checkbox

import Filter
import Selects
import Selectshared
import Shared
import Selectitem

stringFromBool : Bool -> String
stringFromBool value =
  if value then
    "True"
  else
    "False"

trueFromPass : String -> String
trueFromPass value = 
  if value == "Pass" then
    "True"
  else
    "False"

habitatItem : Model -> String
habitatItem model =
  String.join "," <| List.sort (Set.toList ( Set.fromList ( List.map model.selectpost.habitatSearch.itemToLabel model.selectpost.habitatSearch.selected )))
  
taxItem : Model -> String
taxItem model =
  String.join "," <| List.map model.selectpost.taxonomySearch.itemToLabel model.selectpost.taxonomySearch.selected

antifamItem : Model -> String
antifamItem model =
  trueFromPass (String.join "," <| List.map model.selectpost.antifamSearch.itemToLabel model.selectpost.antifamSearch.selected)

terminalItem : Model -> String
terminalItem model =
  trueFromPass (String.join "," <| List.map model.selectpost.terminalSearch.itemToLabel model.selectpost.terminalSearch.selected)

type alias SelectModel =
    { habitatSearch : Selectshared.Model Selectitem.Habitat
    , taxonomySearch : Selectshared.Model Selectitem.Taxonomy
    , antifamSearch : Selectshared.Model Selectitem.Antifam
    , terminalSearch : Selectshared.Model Selectitem.Terminal
    }

type alias Model =
    { selectpost : SelectModel
    , filterpost : Filter.Model
    , ask: Bool
    , popoverState1 : Popover.State
    , popoverState2 : Popover.State
    {- 
      , antifamcontent : String
    , terminalcontent : String-}
    , rnacodecontent : String
    , metatcontent : String
    , riboseqcontent : String
    , metapcontent : String
    , hq: String
    , quality : Bool
    }

type Msg 
    = Search
    | FilterMsg Filter.Msg
    | HabitatSearchMsg (Selectshared.Msg Selectitem.Habitat)
    | TaxonomySearchMsg (Selectshared.Msg Selectitem.Taxonomy)
    | AntifamSearchMsg (Selectshared.Msg Selectitem.Antifam)
    | TerminalSearchMsg (Selectshared.Msg Selectitem.Terminal)
    | NoOp
    | PopoverMsg1 Popover.State
    | PopoverMsg2 Popover.State
    {-
     | SetAntifam String
    | SetTerminal String
    -}
    | SetRnacode String
    | SetmetaT String
    | SetRiboseq String
    | SetmetaP String
    | MyCheckMsg Bool
    | Choose

initialModel : (Model, Cmd Msg)
initialModel =
    let
        (sm, cmd) = Filter.initialState "" "" "" "" "" "" "" "" ""
    in
        ({ selectpost = { habitatSearch = Selectshared.initialModel 
                               { id = "exampleMulti"
                               , available = Selectitem.habitats
                               , itemToLabel = Selectitem.habitattoLabel
                               , selected = [ ]
                               , selectConfig = selectConfigHabitatSearch
                               }
                        , taxonomySearch = Selectshared.initialModel 
                               { id = "exampleEmptySearch"
                               , available = Selectitem.taxonomy
                               , itemToLabel = Selectitem.taxtoLabel
                               , selected = [ ]
                               , selectConfig = selectConfigTaxonomySearch
                               }
                        , antifamSearch = Selectshared.initialModel 
                               { id = "exampleEmptySearch"
                               , available = Selectitem.antifam
                               , itemToLabel = Selectitem.antifamtoLabel
                               , selected = [ ]
                               , selectConfig = selectConfigAntifamSearch
                               }
                        , terminalSearch = Selectshared.initialModel 
                               { id = "exampleEmptySearch"
                               , available = Selectitem.terminal
                               , itemToLabel = Selectitem.terminaltoLabel
                               , selected = [ ]
                               , selectConfig = selectConfigTerminalSearch
                               }
                        }
        , filterpost = sm
        , ask = False
        , popoverState1 = Popover.initialState
        , popoverState2 = Popover.initialState
        {-
        , antifamcontent = ""
        , terminalcontent = ""-}
        , rnacodecontent = ""
        , metatcontent = ""
        , riboseqcontent = ""
        , metapcontent = ""
        , hq = ""
        , quality = False
        }
        , Cmd.map FilterMsg cmd
        )


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
    let
        ifQuery f = 
                let
                    (qmpost, c) = f model.selectpost
                in ({model| selectpost = qmpost}, c)
    in case msg of
        NoOp ->
            ( model, Cmd.none )
        
        PopoverMsg1 state ->
            ( { model | popoverState1 = state }, Cmd.none )

        PopoverMsg2 state ->
            ( { model | popoverState2 = state }, Cmd.none )

        HabitatSearchMsg sub ->
            ifQuery <| \qmodel ->
              let
                ( subModel, subCmd ) =
                    Selectshared.update sub qmodel.habitatSearch
              in
                ({ qmodel | habitatSearch = subModel }
                , Cmd.map HabitatSearchMsg subCmd
                )
        TaxonomySearchMsg sub ->
            ifQuery <| \qmodel ->
              let
                ( subModel, subCmd ) =
                    Selectshared.update sub qmodel.taxonomySearch
              in
                ({ qmodel | taxonomySearch = subModel }
                , Cmd.map TaxonomySearchMsg subCmd
                )

        AntifamSearchMsg sub ->
            ifQuery <| \qmodel ->
              let
                ( subModel, subCmd ) =
                    Selectshared.update sub qmodel.antifamSearch
              in
                ({ qmodel | antifamSearch = subModel }
                , Cmd.map AntifamSearchMsg subCmd
                )

        TerminalSearchMsg sub ->
            ifQuery <| \qmodel ->
              let
                ( subModel, subCmd ) =
                    Selectshared.update sub qmodel.terminalSearch
              in
                ({ qmodel | terminalSearch = subModel }
                , Cmd.map TerminalSearchMsg subCmd
                )

        {-SetAntifam b ->
            ( { model | antifamcontent = b }, Cmd.none )

        SetTerminal b ->
            ( { model | terminalcontent = b }, Cmd.none )-}

        SetRnacode p ->
            ( { model | rnacodecontent = p }, Cmd.none )

        SetmetaT number ->
            ( { model | metatcontent = number }, Cmd.none )  

        SetRiboseq number ->
            ( { model | riboseqcontent = number }, Cmd.none )  

        SetmetaP cov ->
            ( { model | metapcontent = cov }, Cmd.none )  

        MyCheckMsg b ->
            ( { model | hq = stringFromBool b }, Cmd.none )     

        Search ->
            if habitatItem model == "" && taxItem model == "" && antifamItem model == "" && terminalItem model == "" && model.rnacodecontent == "" && model.metatcontent == "" && model.riboseqcontent == "" && model.metapcontent == "" && model.hq == "" then
                (model, Cmd.none)
            else
                let
                    (sm, cmd) = Filter.initialState (habitatItem model) (taxItem model) (antifamItem model) (terminalItem model) model.rnacodecontent model.metatcontent model.riboseqcontent model.metapcontent model.hq
                in ({ model| filterpost = sm, ask=True }, Cmd.map FilterMsg cmd)

        FilterMsg m -> 
            let
                (nqm, cmd) = Filter.update m model.filterpost
            in
                ({ model| filterpost = nqm }, Cmd.map FilterMsg cmd)

        Choose ->
          ( { model | quality = True }, Cmd.none )  

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

viewModel : Model -> Html Msg
viewModel model =
    case model.filterpost.showpost of
    Filter.SLoading ->
        if model.ask == True then
            div [] 
                [ viewSearch model
                , Html.hr [] []
                , Filter.viewModel model.filterpost
                    |> Html.map FilterMsg
                ]
        else
            div [] 
                [ viewSearch model
                ]
    Filter.MultiResults r ->
        div [] 
            [ viewSearch model
            , Html.hr [] []
            , Filter.viewModel model.filterpost
                |> Html.map FilterMsg
            ]
    _ ->
        div []
            [ viewSearch model ]

viewSearch: Model -> Html Msg
viewSearch model = div []
        [ h5 [] [ text "Browse by habitats and taxonomy "] 
        , div [] [ p [] [ label [id "browse"] [ text "Browse by habitats " 
                                              , Popover.config
                                               ( Button.button
                                                   [ Button.small
                                                   , Button.outlineInfo
                                                   , Button.attrs <|
                                                       Popover.onHover model.popoverState2 PopoverMsg2
                                                   ]
                                                   [ span [class "fa fa-question-circle"]
                                                   []
                                                   ]
                                               )
                                               |> Popover.right
                                               |> Popover.content []
                                                      [ text "Habitats with the suffix 'associated' represent a wider range of environments that cannot be traced further to more specific habitats. For example, 'human associated' represents human related habitats other than those listed in 'gut', 'south', etc." ]
                                               |> Popover.view model.popoverState2] ] ]
        , Selectshared.view
            model.selectpost.habitatSearch
            |> Html.map HabitatSearchMsg
        , div [] [ p [] [ label [id "browse"] [ text "Browse by taxonomy" ] ] ]
        , Selectshared.view
            model.selectpost.taxonomySearch
            |> Html.map TaxonomySearchMsg
        , h5 [] [ text "Browse by quality"]
        , div [] [ Form.form [] 
                    [ Form.group []
                        [  Checkbox.checkbox
                           [ Checkbox.id "myChk"
                           , Checkbox.indeterminate
                           , Checkbox.onCheck MyCheckMsg
                           ] 
                           "Only show high quality"
                        ]
                    ]
                 ]
        , div [class "browse"] [Button.button [ Button.info, Button.onClick Choose ] [ text "Specific quality tests" ] ]
        , if model.quality == True then
            viewSpecific model
          else
            p [] [ text "" ]
        , div [class "browse"] [Button.button [ Button.info, Button.onClick Search] [ text "Browse" ]]
        ]

selectConfigHabitat =
    Selects.newConfig
        { onSelect = Selectshared.OnSelect
        , toLabel = Selectitem.habitattoLabel
        , filter = Shared.filter 1 Selectitem.habitattoLabel
        , toMsg = Selectshared.SelectMsg
        }

selectConfigHabitatSearch =
    selectConfigHabitat
        |> Selects.withMultiSelection True
        |> Selects.withOnRemoveItem Selectshared.OnRemoveItem
        -- |> Selects.withCutoff 12
        |> Selects.withEmptySearch True
        |> Selects.withNotFound "No matches"
        |> Selects.withPrompt "Select habitats"

selectConfigTaxonomySearch =
    Selects.newConfig
        { onSelect = Selectshared.OnSingleSelect
        , toLabel = .label
        , filter = Shared.filter 1 .label
        , toMsg = Selectshared.SelectMsg
        }
        -- |> Selects.withCutoff 12
        |> Selects.withEmptySearch True
        |> Selects.withNotFound "No matches"
        |> Selects.withPrompt "Select taxonomy"

selectConfigAntifamSearch =
    Selects.newConfig
        { onSelect = Selectshared.OnSingleSelect
        , toLabel = .label
        , filter = Shared.filter 1 .label
        , toMsg = Selectshared.SelectMsg
        }
        -- |> Selects.withCutoff 12
        |> Selects.withEmptySearch True
        |> Selects.withNotFound "No matches"
        |> Selects.withPrompt "Select antifam"

selectConfigTerminalSearch =
    Selects.newConfig
        { onSelect = Selectshared.OnSingleSelect
        , toLabel = .label
        , filter = Shared.filter 1 .label
        , toMsg = Selectshared.SelectMsg
        }
        -- |> Selects.withCutoff 12
        |> Selects.withEmptySearch True
        |> Selects.withNotFound "No matches"
        |> Selects.withPrompt "Select terminal"

viewSpecific : Model -> Html Msg
viewSpecific model =  
    div []
        [ div [] [ p [] [ label [id "browse"] [ text "Antifam" ] ] ]
        , Selectshared.view
          model.selectpost.antifamSearch
          |> Html.map AntifamSearchMsg
        , div [] [ p [] [ label [id "browse"] [ text "Terminal checking" ] ] ]
          , Selectshared.view
          model.selectpost.terminalSearch
          |> Html.map TerminalSearchMsg
        {-
        div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "If not belong to the Antifam database" ]
                        , Input.text 
                            [ Input.value model.antifamcontent
                            , Input.attrs [ placeholder "BOOL" ] 
                            , Input.onInput SetAntifam
                            ]
                        ]
                    ]
                  ]
        , div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "If pass the Terminal checking" ]
                        , Input.text 
                            [ Input.value model.terminalcontent
                            , Input.attrs [ placeholder "BOOL" ] 
                            , Input.onInput SetTerminal
                            ]
                        ]
                    ]
                  ]
        -}
        , div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "P-value of RNAcode" ]
                        , Input.text 
                            [ Input.value model.rnacodecontent
                            , Input.attrs [ placeholder "FLOAT" ] 
                            , Input.onInput SetRnacode
                            ]
                        ]
                    ]
                  ]
        , div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "The number of mapped samples of metaTranscriptome" ]
                        , Input.text 
                            [ Input.value model.metatcontent
                            , Input.attrs [ placeholder "INT" ] 
                            , Input.onInput SetmetaT
                            ]
                        ]
                    ]
                  ]
        , div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "The number of mapped samples of Riboseq" ]
                        , Input.text 
                            [ Input.value model.riboseqcontent
                            , Input.attrs [ placeholder "INT" ] 
                            , Input.onInput SetRiboseq
                            ]
                        ]
                    ]
                  ]
        , div [] [ Form.form []
                    [ Form.group []
                        [ Form.label [] [ text "The coverage of metaProteome" ]
                        , Input.text 
                            [ Input.value model.metapcontent
                            , Input.attrs [ placeholder "FLOAT ranges from 0-1" ] 
                            , Input.onInput SetmetaP
                            ]
                        ]
                    ]
                  ]
    ]