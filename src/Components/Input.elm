module Components.Input exposing
    ( defaultConfig
    , viewTextOrNumber
    , withDisabled
    , withErrorMessage
    , withLabel
    , withOnBlur
    , withOnChange
    , withType
    , withValue
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Shared.Styles as Styles


type alias Config msg =
    { label : String
    , disabled : Bool
    , type_ : String
    , onChange : Maybe (String -> msg)
    , onBlur : Maybe msg
    , value : String
    , errorMessage : Maybe String
    }


defaultConfig : Config msg
defaultConfig =
    { label = ""
    , disabled = False
    , type_ = "text"
    , onChange = Nothing
    , onBlur = Nothing
    , value = ""
    , errorMessage = Nothing
    }


withOnBlur : Maybe msg -> Config msg -> Config msg
withOnBlur onBlur config =
    { config | onBlur = onBlur }


withLabel : String -> Config msg -> Config msg
withLabel label config =
    { config | label = label }


withDisabled : Bool -> Config msg -> Config msg
withDisabled disabled config =
    { config | disabled = disabled }


withType : String -> Config msg -> Config msg
withType type_ config =
    { config | type_ = type_ }


withOnChange : Maybe (String -> msg) -> Config msg -> Config msg
withOnChange onChange config =
    { config | onChange = onChange }


withValue : String -> Config msg -> Config msg
withValue value config =
    { config | value = value }


withErrorMessage : Maybe String -> Config msg -> Config msg
withErrorMessage errorMessage config =
    { config | errorMessage = errorMessage }


viewTextOrNumber : Config msg -> Html msg
viewTextOrNumber { label, disabled, type_, onChange, onBlur, value, errorMessage } =
    let
        hasError : Bool
        hasError =
            errorMessage /= Nothing
    in
    Html.span
        [ Attrs.class "flex flex-col rounded px-2 py-1"
        , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ) ]
        ]
        [ Html.label [ Attrs.class "text-sm pl-1", Attrs.classList [ ( "text-red-500", hasError ) ] ]
            [ Html.text (errorMessage |> Maybe.withDefault label) ]
        , Html.input
            ([ Attrs.type_ type_
             , Attrs.class "focus:outline-none w-full"
             , Attrs.class Styles.inputBorder
             , Attrs.classList
                [ ( "bg-[#e8f3fc]", disabled )
                , ( "border-red-500", hasError )
                , ( "border-stone-400", not hasError )
                ]
             , Attrs.disabled disabled
             , Attrs.value value
             ]
                ++ (onChange
                        |> Maybe.map (Events.onInput >> List.singleton)
                        |> Maybe.withDefault []
                   )
                ++ (onBlur
                        |> Maybe.map (Events.onBlur >> List.singleton)
                        |> Maybe.withDefault []
                   )
            )
            []
        ]
