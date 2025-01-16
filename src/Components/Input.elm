module Components.Input exposing (viewTextOrNumber)

import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events


viewTextOrNumber :
    { label : String
    , disabled : Bool
    , type_ : String
    , onChange : String -> msg
    , value : String
    , errorMessage : Maybe String
    }
    -> Html msg
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
            [ Attrs.type_ type_
            , Attrs.class "border rounded px-2 py-1 focus:outline-none border-stone-400"
            , Attrs.classList [ ( "bg-[#e8f3fc]", disabled ), ( "border-red-500", hasError ) ]
            , Attrs.disabled disabled
            , Attrs.value value
            , Events.onInput onChange
            ]
            []
        ]
