module Button exposing (new, view, withAriaLabel, withFormId, withIconLeft, withIconRight, withId, withOnClick, withType)

import Html as Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


type Button msg
    = Settings
        { class : String
        , label : String
        , leftIcon : Maybe (Html msg)
        , rightIcon : Maybe (Html msg)
        , onClickMsg : Maybe msg
        , buttonType : String
        , additionalAttributes : List (Html.Attribute msg)
        }


new : { class : String, label : String } -> Button msg
new { class, label } =
    Settings
        { class = class
        , label = label
        , leftIcon = Nothing
        , rightIcon = Nothing
        , onClickMsg = Nothing
        , buttonType = "button"
        , additionalAttributes = []
        }


withFormId : String -> Button msg -> Button msg
withFormId formId (Settings model) =
    Settings
        { model
            | additionalAttributes =
                Attributes.form formId :: model.additionalAttributes
        }


withAriaLabel : String -> Button msg -> Button msg
withAriaLabel label (Settings model) =
    Settings
        { model
            | additionalAttributes =
                Attributes.attribute "aria-label" label :: model.additionalAttributes
        }


withId : String -> Button msg -> Button msg
withId id (Settings model) =
    Settings { model | additionalAttributes = Attributes.id id :: model.additionalAttributes }


withOnClick : msg -> Button msg -> Button msg
withOnClick msg (Settings model) =
    Settings { model | onClickMsg = Just msg }


withIconLeft : Maybe (Html msg) -> Button msg -> Button msg
withIconLeft icon (Settings model) =
    Settings { model | leftIcon = icon }


withIconRight : Maybe (Html msg) -> Button msg -> Button msg
withIconRight icon (Settings model) =
    Settings { model | rightIcon = icon }


withType : String -> Button msg -> Button msg
withType buttonType (Settings model) =
    Settings { model | buttonType = buttonType }


view : Button msg -> Html msg
view (Settings model) =
    Html.button
        ([ Attributes.class (model.class ++ " flex items-center justify-center gap-2")
         , Attributes.type_ model.buttonType
         ]
            ++ model.additionalAttributes
            ++ (case model.onClickMsg of
                    Just msg ->
                        [ Events.onClick msg ]

                    Nothing ->
                        []
               )
        )
        [ case model.leftIcon of
            Just icon ->
                icon

            Nothing ->
                Html.text ""
        , Html.text model.label
        , case model.rightIcon of
            Just icon ->
                icon

            Nothing ->
                Html.text ""
        ]
