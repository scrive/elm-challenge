module Ui.Modal exposing (view)

import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events


type alias Model msg =
    { title : String
    , closeMsg : msg
    }


view : Model msg -> Html msg -> Html msg
view { title, closeMsg } body =
    Html.div
        [ Attributes.class "overflow-y-auto overflow-x-hidden fixed top-0 right-0 left-0 z-50 justify-center items-center w-full md:inset-0 h-[calc(100%-1rem)] max-h-full flex bg-gray-200 bg-opacity-80" ]
        [ Html.div [ Attributes.class "relative p-4 w-full max-w-2xl max-h-full" ]
            [ Html.div [ Attributes.class "relative bg-white rounded-lg shadow-sm dark:bg-gray-700" ]
                [ Html.div [ Attributes.class "flex items-center justify-between p-4 md:p-5 border-b rounded-t dark:border-gray-600 border-gray-200" ]
                    [ Html.text title
                    , Html.div
                        [ Events.onClick closeMsg
                        , Attributes.class "cursor-pointer"
                        ]
                        [ Html.text "X" ]
                    ]
                , Html.div [ Attributes.class "p-4 md:p-5 space-y-4" ] [ body ]
                ]
            ]
        ]
