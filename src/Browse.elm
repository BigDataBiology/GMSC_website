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

import Filter
import Selects
import Selectshared
import Shared
import Selectitem

type alias SelectModel =
    { hq: Bool
    , habitatSearch : Selectshared.Model Selectitem.Habitat
    , taxonomySearch : Selectshared.Model Selectitem.Taxonomy
    }

type alias Model =
    { selectpost : SelectModel
    , filterpost : Filter.Model
    , ask: Bool
    , popoverState1 : Popover.State
    , popoverState2 : Popover.State
    }

type Msg 
    = Search
    | FilterMsg Filter.Msg
    | HabitatSearchMsg (Selectshared.Msg Selectitem.Habitat)
    | TaxonomySearchMsg (Selectshared.Msg Selectitem.Taxonomy)
    | NoOp
    | PopoverMsg1 Popover.State
    | PopoverMsg2 Popover.State

initialModel : (Model, Cmd Msg)
initialModel =
    let
        (sm, cmd) = Filter.initialState "" ""
    in
        ({ selectpost = { hq = True
                   , habitatSearch = Selectshared.initialModel 
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
                   }
        , filterpost = sm
        , ask = False
        , popoverState1 = Popover.initialState
        , popoverState2 = Popover.initialState
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
              
        Search ->
                let (qhabitat,qtaxonomy) = ( (String.join "," <| List.sort (Set.toList ( Set.fromList ( List.map model.selectpost.habitatSearch.itemToLabel model.selectpost.habitatSearch.selected ))))
                                           , (String.join "," <| List.map model.selectpost.taxonomySearch.itemToLabel model.selectpost.taxonomySearch.selected))
                in
                  if qhabitat == "" && qtaxonomy == "" then
                    (model, Cmd.none)
                  else
                    let
                      (sm, cmd) = Filter.initialState qhabitat qtaxonomy
                    in ({ model| filterpost = sm, ask=True }, Cmd.map FilterMsg cmd)

        FilterMsg m -> 
            let
                (nqm, cmd) = Filter.update m model.filterpost
            in
                ({ model| filterpost = nqm }, Cmd.map FilterMsg cmd)

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

viewModel : Model -> Html Msg
viewModel model =
    case model.filterpost of
    Filter.Loading ->
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
    Filter.Results r ->
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
        [ h5 [] [ text "Browse by habitats and taxonomy "
                , Popover.config
                    ( Button.button
                    [ Button.small
                    , Button.outlineInfo
                    , Button.attrs <|
                        Popover.onHover model.popoverState1 PopoverMsg1
                    ]
                    [ span [class "fa fa-question-circle"][]]
                    )
                        |> Popover.right
                        |> Popover.content [] [ text "Only browse high quality 90AA small protein families which can pass all computational quality tests and show experimental evidances." ]
                        |> Popover.view model.popoverState1
                ] 
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
        , div [class "browse"] [Button.button [ Button.info, Button.onClick Search] [ text "Browse" ]]
        ]

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