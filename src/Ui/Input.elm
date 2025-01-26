module Ui.Input exposing (viewWithLabel)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Ui.Label as Label
import Util.Validate as Validate


type alias Model r inputId msg =
    { r
        | msg : inputId -> String -> msg
        , placeholder : String
        , value : String
        , errors : Validate.Errors
        , inputId : inputId
        , inputIdToString : inputId -> String
    }


baseCssClasses : String
baseCssClasses =
    "shadow-sm bg-white-50 border border-gray-300 text-gray-900 text-sm rounded-lg focus:ring-blue-500 focus:border-blue-500 block w-full p-2.5 dark:bg-white-700 dark:border-gray-600 dark:placeholder-gray-400 dark:text-white dark:focus:ring-blue-500 dark:focus:border-blue-500 dark:shadow-sm-light"


view : Model r inputId msg -> Html msg
view { msg, placeholder, value, errors, inputId, inputIdToString } =
    Html.div []
        [ Html.input
            [ Attributes.type_ "text"
            , Attributes.class baseCssClasses
            , Attributes.placeholder placeholder
            , Attributes.value value
            , Attributes.id <| inputIdToString inputId
            , Events.onInput <| msg inputId
            ]
            []
        , errors
            |> List.map errorView
            |> Html.div []
        ]


errorCssClasses : String
errorCssClasses =
    "mt-1 text-xs text-red-500"


errorView : String -> Html msg
errorView error =
    Html.div
        [ Attributes.class errorCssClasses ]
        [ Html.text error ]


viewWithLabel : List (Attribute msg) -> Model { labelText : String } inputId msg -> Html msg
viewWithLabel wrapperAttributes ({ labelText, inputIdToString, inputId } as model) =
    Html.div wrapperAttributes
        [ Label.view
            { htmlFor = inputIdToString inputId
            , text = labelText
            }
        , view model
        ]
