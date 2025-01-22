module Ui.Button exposing (submitButtonView)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model r msg =
    { r
        | text : String
        , msg : msg
    }


baseCssClasses : String
baseCssClasses =
    "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-sm px-5 py-2.5 text-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"


view : List (Attribute msg) -> Model r msg -> Html msg
view attributes { text, msg } =
    Html.button
        ([ Events.onClick msg
         , Attributes.class baseCssClasses
         ]
            ++ attributes
        )
        [ Html.text text ]


submitButtonView : Model r msg -> Html msg
submitButtonView =
    view [ Attributes.type_ "submit" ]
