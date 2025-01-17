module Components.Input exposing
    ( disabled
    , error
    , id
    , label
    , onBlur
    , onChange
    , type_
    , value
    , viewTextOrNumber
    )

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events


type alias Config msg =
    { label : String
    , disabled : Bool
    , type_ : String
    , onChange : Maybe (String -> msg)
    , onBlur : Maybe msg
    , value : String
    , error : String
    , id : String
    }


defaultConfig : Config msg
defaultConfig =
    { label = ""
    , disabled = False
    , type_ = "text"
    , onChange = Nothing
    , onBlur = Nothing
    , value = ""
    , error = ""
    , id = ""
    }


id : String -> Config msg -> Config msg
id id_ config =
    { config | id = id_ }


onBlur : Maybe msg -> Config msg -> Config msg
onBlur onBlur_ config =
    { config | onBlur = onBlur_ }


label : String -> Config msg -> Config msg
label label_ config =
    { config | label = label_ }


disabled : Bool -> Config msg -> Config msg
disabled disabled_ config =
    { config | disabled = disabled_ }


type_ : String -> Config msg -> Config msg
type_ type__ config =
    { config | type_ = type__ }


onChange : Maybe (String -> msg) -> Config msg -> Config msg
onChange onChange_ config =
    { config | onChange = onChange_ }


value : String -> Config msg -> Config msg
value value_ config =
    { config | value = value_ }


error : String -> Config msg -> Config msg
error error_ config =
    { config | error = error_ }


viewTextOrNumber : List (Config msg -> Config msg) -> Html msg
viewTextOrNumber customConfigurations =
    let
        config : Config msg
        config =
            customConfigurations
                |> List.foldl
                    (\customConfiguration config_ -> customConfiguration config_)
                    defaultConfig

        hasError : Bool
        hasError =
            not (String.isEmpty config.error)
    in
    Html.span
        [ Attrs.class "flex flex-col rounded px-2 py-1"
        , Attrs.classList [ ( "bg-[#e8f3fc]", config.disabled ) ]
        ]
        [ Html.label [ Attrs.class "text-sm pl-1", Attrs.classList [ ( "text-red-500", hasError ) ] ]
            [ Html.text
                (if hasError then
                    config.error

                 else
                    config.label
                )
            ]
        , Html.input
            ([ Attrs.type_ config.type_
             , Attrs.id config.id
             , Attrs.class "focus:outline-none w-full"
             , Attrs.class "border rounded px-2 py-1"
             , Attrs.classList
                [ ( "bg-[#e8f3fc]", config.disabled )
                , ( "border-red-500", hasError )
                , ( "border-stone-400", not hasError )
                ]
             , Attrs.disabled config.disabled
             , Attrs.value config.value
             ]
                ++ (config.onChange
                        |> Maybe.map (Events.onInput >> List.singleton)
                        |> Maybe.withDefault []
                   )
                ++ (config.onBlur
                        |> Maybe.map (Events.onBlur >> List.singleton)
                        |> Maybe.withDefault []
                   )
            )
            []
        ]
