module Status exposing (loading)

import Html exposing (Html, div, p, text)
import Html.Attributes as HtmlAttr


loading : String -> String -> Html msg
loading title description =
    div [ HtmlAttr.class "status-panel status-panel-loading" ]
        [ p [ HtmlAttr.class "status-title" ] [ text title ]
        , p [ HtmlAttr.class "status-copy" ] [ text description ]
        ]
