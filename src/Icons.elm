module Icons exposing (..)

import Css exposing (ms)
import Html exposing (Html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


arrowDown : Html msg
arrowDown =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M19.5 8.25l-7.5 7.5-7.5-7.5" ] [] ]


cautionIcon : Html msg
cautionIcon =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M12 9v3.75m-9.303 3.376c-.866 1.5.217 3.374 1.948 3.374h14.71c1.73 0 2.813-1.874 1.948-3.374L13.949 3.378c-.866-1.5-3.032-1.5-3.898 0L2.697 16.126zM12 15.75h.007v.008H12v-.008z" ] [] ]


plusIcon : Html msg
plusIcon =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M12 6v12m6-6H6" ] [] ]


deleteIcon : Html msg
deleteIcon =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M14.74 9l-.346 9m-4.788 0L9.26 9m9.968-3.21c.342.052.682.107 1.022.166m-1.022-.165L18.16 19.673a2.25 2.25 0 01-2.244 2.077H8.084a2.25 2.25 0 01-2.244-2.077L4.772 5.79m14.456 0a48.108 48.108 0 00-3.478-.397m-12 .562c.34-.059.68-.114 1.022-.165m0 0a48.11 48.11 0 013.478-.397m7.5 0v-.916c0-1.18-.91-2.164-2.09-2.201a51.964 51.964 0 00-3.32 0c-1.18.037-2.09 1.022-2.09 2.201v.916m7.5 0a48.667 48.667 0 00-7.5 0" ] [] ]


editIcon : Html msg
editIcon =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M16.862 4.487l1.687-1.688a1.875 1.875 0 112.652 2.652L10.582 16.07a4.5 4.5 0 01-1.897 1.13L6 18l.8-2.685a4.5 4.5 0 011.13-1.897l8.932-8.931zm0 0L19.5 7.125M18 14v4.75A2.25 2.25 0 0115.75 21H5.25A2.25 2.25 0 013 18.75V8.25A2.25 2.25 0 015.25 6H10" ] [] ]


checkIcon : Html ms
checkIcon =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M4.5 12.75l6 6 9-13.5" ] [] ]
