module Form.Tags exposing (..)

import Either exposing (Either(..))
import Either as Either

import Data.Tag exposing (Tag, TagToRemove)
import Data.Tag as Tag

import Html exposing (Html)
import Html as Html


viewToRemove : TagToRemove -> Html msg
viewToRemove ttr =
    Html.span
        []
        [ Html.text <| Tag.nameOfRemoved ttr ]


viewTag : Tag -> Html msg
viewTag tag =
    Html.span
        []
        [ Html.text <| Tag.nameOf tag ++ Tag.valueOf tag ]


view : Either TagToRemove Tag -> Html msg
view = Either.unpack viewToRemove viewTag