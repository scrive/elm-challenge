module Icons exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)


arrowDown =
    svg [ fill "none", viewBox "0 0 24 24", strokeWidth "1.5", stroke "currentColor" ] [ Svg.path [ strokeLinecap "round", strokeLinejoin "round", d "M19.5 8.25l-7.5 7.5-7.5-7.5" ] [] ]
