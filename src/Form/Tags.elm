module Form.Tags exposing (..)

import Either exposing (Either(..))
import Either as Either

import Data.Tag exposing (Tag, TagToRemove)
import Data.Tag as Tag

import Html exposing (Html)
import Html as Html
import Html.Attributes as Attrs
import Html.Events as Evt

import Validate exposing (Valid)


type TagInProgress
    = Changing Int { newValue : String }
    | Restoring Int { newValue : String }
    | Creating { newName : String, newValue : String }


type alias Handlers msg =
    { onRestore : TagToRemove -> msg
    , onRemove : Tag -> msg
    , onChange : Tag -> Valid { value : String } -> msg
    , onCreate : Valid { name : String, value : String } -> msg
    , markInProgress : TagInProgress -> msg
    }


viewToRemove : Handlers msg -> Int -> TagToRemove -> Html msg
viewToRemove { onRestore } idx ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfRemoved ttr
        , Html.button
            [ Evt.onClick <| onRestore ttr ]
            [ Html.text "(Restore)" ]
        ]


viewTag : Handlers msg -> Int -> Tag -> Html msg
viewTag { onRemove, markInProgress } idx tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag ++ " : " ++ Tag.valueOf tag
        , Html.button
            [ Evt.onClick <| markInProgress <| Changing idx { newValue = Tag.valueOf tag } ]
            [ Html.text "(Update value)" ]
        , Html.button
            [ Evt.onClick <| onRemove tag ]
            [ Html.text "(Remove)" ]
        ]


view : Handlers msg -> Maybe TagInProgress -> List (Either TagToRemove Tag) -> Html msg
view handlers mbInProgress items =
    Html.ul
        [ ]
        <| List.indexedMap
            (\idx ->
                Either.unpack (viewToRemove handlers idx) (viewTag handlers idx)
            ) items
        ++
        [ case mbInProgress of
            Just (Creating { newName, newValue }) ->
                Html.div
                        []
                        [ Html.input
                            [ Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = str, newValue = "" }
                            , Attrs.class "border-black border-solid border-2"
                            ]
                            [ Html.text newName ]
                        , Html.input
                            [ Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = newName, newValue = str }
                            , Attrs.class "border-black border-solid border-2"
                            ]
                            [ Html.text newValue ]
                        , Html.button
                            [ -- Evt.onClick
                            ]
                            [ Html.text "Submit" ]
                        ]
            Just _ ->
                Html.text ""
            Nothing ->
                Html.button
                    [ Evt.onClick <| handlers.markInProgress <| Creating { newName = "", newValue = "" } ]
                    [ Html.text "Add" ]
        ]