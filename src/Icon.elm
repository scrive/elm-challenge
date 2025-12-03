module Icon exposing (pencilIcon, saveIcon, trashIcon)

import Html exposing (Html)
import Html.Attributes as Attributes
import Svg as Svg
import Svg.Attributes as SvgAttr


saveIcon : Html Never
saveIcon =
    Html.span [ Attributes.attribute "aria-hidden" "true" ]
        [ Svg.svg
            [ SvgAttr.width "24"
            , SvgAttr.height "24"
            , SvgAttr.viewBox "0 0 24 24"
            ]
            [ Svg.g
                [ SvgAttr.fill "none"
                , SvgAttr.stroke "currentColor"
                , SvgAttr.strokeLinecap "round"
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.strokeWidth "2"
                ]
                [ Svg.path
                    [ SvgAttr.d "M3 12c0 -4.97 4.03 -9 9 -9c4.97 0 9 4.03 9 9c0 4.97 -4.03 9 -9 9c-4.97 0 -9 -4.03 -9 -9Z"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M8 12l3 3l5 -5"
                    ]
                    []
                ]
            ]
        ]


pencilIcon : Html Never
pencilIcon =
    Html.span [ Attributes.attribute "aria-hidden" "true" ]
        [ Svg.svg
            [ SvgAttr.width "24"
            , SvgAttr.height "24"
            , SvgAttr.viewBox "0 0 24 24"
            ]
            [ Svg.g
                [ SvgAttr.fill "none"
                , SvgAttr.stroke "currentColor"
                , SvgAttr.strokeLinecap "round"
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.strokeWidth "2"
                ]
                [ Svg.path
                    [ SvgAttr.d "M3 21l2 -6l11 -11c1 -1 3 -1 4 0c1 1 1 3 0 4l-11 11l-6 2"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M15 5l4 4"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.strokeWidth "1"
                    , SvgAttr.d "M6 15l3 3"
                    ]
                    []
                ]
            ]
        ]


trashIcon : Html Never
trashIcon =
    Html.span [ Attributes.attribute "aria-hidden" "true" ]
        [ Svg.svg
            [ SvgAttr.width "24"
            , SvgAttr.height "24"
            , SvgAttr.viewBox "0 0 24 24"
            ]
            [ Svg.g
                [ SvgAttr.fill "none"
                , SvgAttr.stroke "currentColor"
                , SvgAttr.strokeLinecap "round"
                , SvgAttr.strokeLinejoin "round"
                , SvgAttr.strokeWidth "2"
                ]
                [ Svg.path
                    [ SvgAttr.d "M12 20h5c0.5 0 1 -0.5 1 -1v-14M12 20h-5c-0.5 0 -1 -0.5 -1 -1v-14"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M4 5h16"
                    ]
                    []
                , Svg.path
                    [ SvgAttr.d "M10 4h4M10 9v7M14 9v7"
                    ]
                    []
                ]
            ]
        ]
