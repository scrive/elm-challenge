module View exposing (view)

import Html exposing (Html)
import Model exposing (Model)
import Msg exposing (Msg)
import Utils exposing (classList)


view : Model -> Html Msg
view _ =
    Html.div [ stylesContainer ]
        [ header "UserGroup Form"
        , subheader "Settings"
        , subheader "Contact Details"
        , subheader "Tags"
        ]


header : String -> Html msg
header text =
    Html.span [ stylesHeader ]
        [ Html.text text ]


subheader : String -> Html msg
subheader text =
    Html.span [ stylesSubheader ]
        [ Html.text text ]



-- Styles


stylesContainer : Html.Attribute msg
stylesContainer =
    classList
        [ "flex"
        , "flex-col"
        , "w-[1024px]"
        , "items-center"
        , "mx-auto"
        , "mt-16"
        , "mb-48"
        ]


stylesHeader : Html.Attribute msg
stylesHeader =
    classList
        [ "p-2"
        , "text-5xl"
        , "font-extrabold"
        , "text-transparent"
        , "bg-clip-text"
        , "bg-gradient-to-br"
        , "from-slate-400"
        , "to-slate-800"
        ]


stylesSubheader : Html.Attribute msg
stylesSubheader =
    classList
        [ "p-2"
        , "text-2xl"
        , "font-extrabold"
        , "text-slate-800"
        ]
