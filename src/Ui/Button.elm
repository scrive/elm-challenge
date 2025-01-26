module Ui.Button exposing (extraSmall, medium, submitButtonView, view)

import Html exposing (Attribute, Html)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model r =
    { r
        | text : String
        , size : Size
    }


type Size
    = ExtraSmall
    | Medium


baseCssClasses : String
baseCssClasses =
    "text-white bg-blue-700 hover:bg-blue-800 focus:ring-4 focus:outline-none focus:ring-blue-300 font-medium rounded-lg text-center dark:bg-blue-600 dark:hover:bg-blue-700 dark:focus:ring-blue-800"


rawView : List (Attribute msg) -> Model r -> Html msg
rawView attributes { text, size } =
    Html.button
        ([ Attributes.class baseCssClasses
         , sizeToAttribute size
         ]
            ++ attributes
        )
        [ Html.text text ]


sizeToAttribute : Size -> Attribute msg
sizeToAttribute size =
    case size of
        ExtraSmall ->
            Attributes.class "text-xs px-2 py-1"

        Medium ->
            Attributes.class "text-base px-3 py-2"


submitButtonView : List (Attribute msg) -> Model {} -> Html msg
submitButtonView attributes =
    rawView (Attributes.type_ "submit" :: attributes)


view : List (Attribute msg) -> Model { msg : msg } -> Html msg
view attributes ({ msg } as model) =
    rawView
        ([ Attributes.type_ "button"
         , Events.onClick msg
         ]
            ++ attributes
        )
        model


medium : Size
medium =
    Medium


extraSmall : Size
extraSmall =
    ExtraSmall
