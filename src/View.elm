module View exposing (view)

import Html exposing (Attribute, Html, div, span, text)
import Model exposing (Model)
import Msg exposing (Msg)
import Utils exposing (classList)
import Views.Form as Form


view : Model -> Html Msg
view model =
    div [ stylesContainer ]
        [ header "User Group"
        , subheader "Contact Details"
        , Form.view model.form
        ]


header : String -> Html Msg
header text_ =
    span [ stylesHeader ]
        [ text text_ ]


subheader : String -> Html Msg
subheader text_ =
    span [ stylesSubheader ]
        [ text text_ ]



-- Styles


stylesContainer : Attribute Msg
stylesContainer =
    classList
        [ "grid"
        , "gap-6"
        , "max-w-sm"
        , "items-center"
        , "mx-auto"
        , "my-16"
        ]


stylesHeader : Attribute Msg
stylesHeader =
    classList
        [ "text-5xl"
        , "font-bold"
        ]


stylesSubheader : Attribute Msg
stylesSubheader =
    classList
        [ "text-2xl"
        , "font-bold"
        ]
