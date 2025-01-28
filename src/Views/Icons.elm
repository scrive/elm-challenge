module Views.Icons exposing (..)

import Html exposing (Html)
import Html.Attributes as Attrs
import Svg
import Svg.Attributes as SvgAttrs


isIconSelectedClass : Bool -> String
isIconSelectedClass isSelected =
    if isSelected then
        "text-blue-600"

    else
        "text-gray-400 group-hover:text-gray-500"


settingsIcon : Bool -> Html msg
settingsIcon isSelected =
    Svg.svg
        [ SvgAttrs.class <| "w-4 h-4 me-2 " ++ isIconSelectedClass isSelected
        , Attrs.attribute "aria-hidden" "true"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.viewBox "0 0 20 20"
        ]
        [ Svg.path [ SvgAttrs.d "M5 11.424V1a1 1 0 1 0-2 0v10.424a3.228 3.228 0 0 0 0 6.152V19a1 1 0 1 0 2 0v-1.424a3.228 3.228 0 0 0 0-6.152ZM19.25 14.5A3.243 3.243 0 0 0 17 11.424V1a1 1 0 0 0-2 0v10.424a3.227 3.227 0 0 0 0 6.152V19a1 1 0 1 0 2 0v-1.424a3.243 3.243 0 0 0 2.25-3.076Zm-6-9A3.243 3.243 0 0 0 11 2.424V1a1 1 0 0 0-2 0v1.424a3.228 3.228 0 0 0 0 6.152V19a1 1 0 1 0 2 0V8.576A3.243 3.243 0 0 0 13.25 5.5Z" ] []
        ]


contactIcon : Bool -> Html msg
contactIcon isSelected =
    Svg.svg
        [ SvgAttrs.class <| "w-4 h-4 me-2 " ++ isIconSelectedClass isSelected
        , Attrs.attribute "aria-hidden" "true"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.viewBox "0 0 20 20"
        ]
        [ Svg.path [ SvgAttrs.d "M10 0a10 10 0 1 0 10 10A10.011 10.011 0 0 0 10 0Zm0 5a3 3 0 1 1 0 6 3 3 0 0 1 0-6Zm0 13a8.949 8.949 0 0 1-4.951-1.488A3.987 3.987 0 0 1 9 13h2a3.987 3.987 0 0 1 3.951 3.512A8.949 8.949 0 0 1 10 18Z" ] []
        ]


tagIcon : Bool -> Html msg
tagIcon isSelected =
    Svg.svg
        [ SvgAttrs.class <| "w-4 h-4 me-2 " ++ isIconSelectedClass isSelected
        , Attrs.attribute "aria-hidden" "true"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.viewBox "0 0 448 512"
        ]
        [ Svg.path [ SvgAttrs.d "M0 80L0 229.5c0 17 6.7 33.3 18.7 45.3l176 176c25 25 65.5 25 90.5 0L418.7 317.3c25-25 25-65.5 0-90.5l-176-176c-12-12-28.3-18.7-45.3-18.7L48 32C21.5 32 0 53.5 0 80zm112 32a32 32 0 1 1 0 64 32 32 0 1 1 0-64z" ] []
        ]


trashIcon : Html msg
trashIcon =
    Svg.svg
        [ SvgAttrs.class <| "w-4 h-4 me-2"
        , Attrs.attribute "aria-hidden" "true"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.viewBox "0 0 448 512"
        ]
        [ Svg.path [ SvgAttrs.d "M135.2 17.7L128 32 32 32C14.3 32 0 46.3 0 64S14.3 96 32 96l384 0c17.7 0 32-14.3 32-32s-14.3-32-32-32l-96 0-7.2-14.3C307.4 6.8 296.3 0 284.2 0L163.8 0c-12.1 0-23.2 6.8-28.6 17.7zM416 128L32 128 53.2 467c1.6 25.3 22.6 45 47.9 45l245.8 0c25.3 0 46.3-19.7 47.9-45L416 128z" ] []
        ]


errorIcon : Html msg
errorIcon =
    Svg.svg
        [ SvgAttrs.class "flex-shrink-0 inline w-4 h-4 me-3"
        , Attrs.attribute "aria-hidden" "true"
        , SvgAttrs.fill "currentColor"
        , SvgAttrs.viewBox "0 0 20 20"
        ]
        [ Svg.path [ SvgAttrs.d "M10 .5a9.5 9.5 0 1 0 9.5 9.5A9.51 9.51 0 0 0 10 .5ZM9.5 4a1.5 1.5 0 1 1 0 3 1.5 1.5 0 0 1 0-3ZM12 15H8a1 1 0 0 1 0-2h1v-3H8a1 1 0 0 1 0-2h2a1 1 0 0 1 1 1v4h1a1 1 0 0 1 0 2Z" ] []
        ]
