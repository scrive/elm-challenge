module Shared.Icon exposing (IconType(..), view)

import Html exposing (Html)
import Svg
import Svg.Attributes as SvgAttrs


type IconType
    = Add
    | Edit
    | Checkmark
    | Refresh
    | Remove


view : IconType -> Html msg
view =
    iconTypeToSvg


iconTypeToSvg : IconType -> Html msg
iconTypeToSvg iconType =
    case iconType of
        Add ->
            Svg.svg
                [ SvgAttrs.class "w-5 h-5"
                , SvgAttrs.fill "none"
                , SvgAttrs.stroke "currentColor"
                , SvgAttrs.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeLinejoin "round"
                    , SvgAttrs.strokeWidth "2"
                    , SvgAttrs.d "M12 4v16m8-8H4"
                    ]
                    []
                ]

        Edit ->
            Svg.svg
                [ SvgAttrs.class "w-4 h-4"
                , SvgAttrs.fill "none"
                , SvgAttrs.stroke "currentColor"
                , SvgAttrs.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeLinejoin "round"
                    , SvgAttrs.strokeWidth "2"
                    , SvgAttrs.d "M15.232 5.232l3.536 3.536M16 3l5 5-12 12H4v-4L16 3z"
                    ]
                    []
                ]

        Checkmark ->
            Svg.svg
                [ SvgAttrs.class "w-4 h-4"
                , SvgAttrs.fill "none"
                , SvgAttrs.stroke "currentColor"
                , SvgAttrs.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeLinejoin "round"
                    , SvgAttrs.strokeWidth "2"
                    , SvgAttrs.d "M5 13l4 4L19 7"
                    ]
                    []
                ]

        Refresh ->
            Svg.svg
                [ SvgAttrs.class "w-6 h-6"
                , SvgAttrs.fill "currentColor"
                , SvgAttrs.viewBox "0 0 16 16"
                ]
                [ Svg.path
                    [ SvgAttrs.d "M8 3a5 5 0 1 0 4.546 2.914.5.5 0 0 1 .908-.417A6 6 0 1 1 8 2v1z"
                    ]
                    []
                , Svg.path
                    [ SvgAttrs.d "M8 4.466V.534a.25.25 0 0 1 .41-.192l2.36 1.966c.12.1.12.284 0 .384L8.41 4.658A.25.25 0 0 1 8 4.466z"
                    ]
                    []
                ]

        Remove ->
            Svg.svg
                [ SvgAttrs.class "w-4 h-4"
                , SvgAttrs.fill "none"
                , SvgAttrs.stroke "currentColor"
                , SvgAttrs.viewBox "0 0 24 24"
                ]
                [ Svg.path
                    [ SvgAttrs.strokeLinecap "round"
                    , SvgAttrs.strokeLinejoin "round"
                    , SvgAttrs.strokeWidth "2"
                    , SvgAttrs.d "M6 18L18 6M6 6l12 12"
                    ]
                    []
                ]
