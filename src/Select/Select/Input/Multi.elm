module Select.Select.Input.Multi exposing (view)

import Html exposing (Html, div, input, text)
import Html.Attributes as HtmlAttr
import Select.Config exposing (Config)
import Select.Messages as Msg exposing (Msg)
import Select.Models exposing (State)
import Select.Select.RemoveItem as RemoveItem
import Select.Shared as Shared exposing (classNames)


view :
    Config msg item
    -> State
    -> List item
    -> List item
    -> Maybe (List item)
    -> List (Html msg)
view config model availableItems selected maybeMatchedItems =
    let
        val =
            model.query |> Maybe.withDefault ""
    in
    [ currentSelection
        config
        selected
    , input
        (Shared.inputAttributes config model availableItems selected maybeMatchedItems
            ++ [ HtmlAttr.value val ]
            ++ (if List.isEmpty selected then
                    [ HtmlAttr.placeholder config.prompt ]

                else
                    []
               )
        )
        []
    ]


currentSelection config selected =
    div
        ([ HtmlAttr.class classNames.multiInputItemContainer ]
            ++ config.multiInputItemContainerAttrs
        )
        (List.map
            (currentSelection_item config)
            selected
        )


currentSelection_item config item =
    div
        ([ HtmlAttr.class classNames.multiInputItem ]
            ++ config.multiInputItemAttrs
        )
        [ div
            [ HtmlAttr.class classNames.multiInputItemText ]
            [ text (config.toLabel item) ]
        , currentSelection_item_maybeClear
            config
            item
        ]


currentSelection_item_maybeClear config item =
    case config.onRemoveItem of
        Nothing ->
            text ""

        Just _ ->
            currentSelection_item_clear
                config
                item


currentSelection_item_clear config item =
    div
        [ HtmlAttr.class classNames.multiInputItemRemove
        , Shared.onClickWithoutPropagation (Msg.OnRemoveItem item)
            |> HtmlAttr.map config.toMsg
        ]
        [ RemoveItem.view config ]
