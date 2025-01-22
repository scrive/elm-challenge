module Ui.Input exposing (viewWithLabel)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Ui.Label as Label


type alias Model r msg =
    { r
        | msg : String -> msg
        , placeholder : String
        , value : String
        , errors : List String
    }


baseCssClasses : String
baseCssClasses =
    "shadow-sm bg-gray-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-gray-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500 dark:shadow-sm-light"


view : List (Attribute msg) -> Model r msg -> Html msg
view attributes { msg, placeholder, value, errors } =
    Html.div []
        [ Html.input
            ([ Attributes.type_ "text"
             , Attributes.class baseCssClasses
             , Attributes.placeholder placeholder
             , Attributes.value value
             , Events.onInput msg
             ]
                ++ attributes
            )
            []
        , errors
            |> List.map errorView
            |> Html.div []
        ]


errorCssClasses : String
errorCssClasses =
    "mt-2 hidden text-sm text-red-500 peer-[&:not(:placeholder-shown):not(:focus):invalid]:block"


errorView : String -> Html msg
errorView error =
    Html.span
        [ Attributes.class errorCssClasses ]
        [ Html.text error ]


viewWithLabel : Model { inputId : String, labelText : String } msg -> Html msg
viewWithLabel ({ inputId, labelText } as model) =
    Html.div [ Attributes.class "mb-5" ]
        [ view [ Attributes.id inputId ] model
        , Label.view
            { inputId = inputId
            , text = labelText
            }
        ]
