module Components.Input exposing
    ( defaultConfig
    , viewTextOrNumber
    , withDisabled
    , withErrorMessage
    , withLabel
    , withOnChange
    , withType
    , withValue
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events


type alias Config msg =
    { label : String
    , disabled : Bool
    , type_ : String
    , onChange : Maybe (String -> msg)
    , value : String
    , errorMessage : Maybe String
    }


defaultConfig : Config msg
defaultConfig =
    { label = ""
    , disabled = False
    , type_ = "text"
    , onChange = Nothing
    , value = ""
    , errorMessage = Nothing
    }


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
viewTextOrNumber { label, disabled, type_, onChange, value, errorMessage } =
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
             , Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400 w-full"
             , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ), ( "border-red-500", hasError ) ]
             , Attrs.disabled disabled
             , Attrs.value value
             ]
                ++ (onChange
                        |> Maybe.map (Events.onInput >> List.singleton)
                        |> Maybe.withDefault []
                   )
            )
            []
        ]
