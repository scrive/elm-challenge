module Shared.Button exposing (Button, new, view, withClass, withIcon, withLabel, withTooltip)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Shared.Icon as Icon
import Shared.Tooltip as Tooltip


type alias Button msg =
    { onClickMsg : msg
    , icon : Maybe Icon.IconType
    , tooltipText : Maybe String
    , class : String
    , label : Maybe String
    }


new : msg -> Button msg
new msg =
    { onClickMsg = msg
    , icon = Nothing
    , tooltipText = Nothing
    , class = ""
    , label = Nothing
    }


withIcon : Icon.IconType -> Button msg -> Button msg
withIcon iconType button =
    { button | icon = Just iconType }


withTooltip : String -> Button msg -> Button msg
withTooltip text button =
    { button | tooltipText = Just text }


withClass : String -> Button msg -> Button msg
withClass class button =
    { button | class = class }


withLabel : String -> Button msg -> Button msg
withLabel text button =
    { button | label = Just text }


view : Button msg -> Html msg
view { onClickMsg, tooltipText, icon, label, class } =
    let
        ( tooltipId, tooltipAttributes ) =
            let
                tooltipId_ =
                    "tooltip-" ++ Maybe.withDefault "button" label
            in
            Maybe.map (( tooltipId_, [ Attrs.attribute "aria-describedby" tooltipId_ ] ) |> always) tooltipText
                |> Maybe.withDefault ( "", [] )

        content =
            case ( icon, label ) of
                ( Just icon_, Just label_ ) ->
                    [ Icon.view icon_, Html.text label_ ]

                ( Just icon_, Nothing ) ->
                    [ Icon.view icon_ ]

                ( Nothing, Just label_ ) ->
                    [ Html.text label_ ]

                ( Nothing, Nothing ) ->
                    []
    in
    (Html.button
        ([ Attrs.type_ "button"
         , Attrs.class class
         , Attrs.attribute "aria-label" (Maybe.withDefault "Button" label)
         ]
            ++ tooltipAttributes
            ++ [ Events.onClick onClickMsg ]
        )
        content
        :: (Maybe.map (Tooltip.view tooltipId >> List.singleton) tooltipText |> Maybe.withDefault [])
    )
        |> Html.div [ Attrs.class "relative flex items-center group" ]
