module Shared.Button exposing (Button, ButtonMsg(..), new, view, withClass, withIcon, withLabel, withTooltip)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Shared.Icon as Icon
import Shared.Tooltip as Tooltip


type alias Button msg =
    { msg : ButtonMsg msg
    , icon : Maybe Icon.IconType
    , tooltipText : Maybe String
    , class : String
    , label : Maybe String
    }


type ButtonMsg msg
    = OnClick msg
    | OnSubmit msg


new : ButtonMsg msg -> Button msg
new msg =
    { msg = msg
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
view { msg, tooltipText, icon, label, class } =
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

        ( clickAttrs, typeAttr ) =
            case msg of
                OnClick msg_ ->
                    ( [ Events.onClick msg_ ], Attrs.type_ "button" )

                OnSubmit _ ->
                    ( [], Attrs.type_ "submit" )
    in
    (Html.button
        ([ typeAttr
         , Attrs.class class
         , Attrs.attribute "aria-label" (Maybe.withDefault "Button" label)
         ]
            ++ tooltipAttributes
            ++ clickAttrs
        )
        content
        :: (Maybe.map (Tooltip.view tooltipId >> List.singleton) tooltipText |> Maybe.withDefault [])
    )
        |> Html.div [ Attrs.class "relative flex items-center group" ]
