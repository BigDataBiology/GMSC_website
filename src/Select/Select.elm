module Select.Select exposing (view)

import Html exposing (Html, div)
import Html.Attributes as HtmlAttr
import Select.Config exposing (Config)
import Select.Models exposing (State)
import Select.Search as Search
import Select.Select.Input
import Select.Select.Menu
import Select.Shared as Shared exposing (classNames)


view : Config msg item -> State -> List item -> List item -> Html msg
view config state availableItems selectedItems =
    let
        availableItemsWithCustom =
            case config.customInput of
                Nothing ->
                    availableItems

                Just fn ->
                    case state.query of
                        Nothing ->
                            availableItems

                        Just query ->
                            if String.isEmpty query then
                                availableItems

                            else
                                let
                                    items : List item
                                    items =
                                        query
                                            |> Shared.splitWithSeparators config.valueSeparators
                                            |> List.map fn
                                in
                                items ++ availableItems

        -- This is a maybe because
        -- When no search has been done we don't want to show the menu
        -- Which is different from a search that returns empty
        maybeMatchedItems : Maybe (List item)
        maybeMatchedItems =
            Search.matchedItemsWithCutoff
                config
                state.query
                availableItemsWithCustom
                selectedItems
    in
    div
        [ HtmlAttr.id state.id
        , HtmlAttr.class classNames.root
        ]
        [ Select.Select.Input.view
            config
            state
            availableItems
            selectedItems
            maybeMatchedItems
        , div [ HtmlAttr.class classNames.underlineWrapper ]
            [ div [ HtmlAttr.class classNames.underline ] []
            ]
        , Select.Select.Menu.view
            config
            state
            maybeMatchedItems
            selectedItems
        ]
