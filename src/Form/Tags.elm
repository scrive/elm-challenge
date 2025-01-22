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
    { tryRemove : Tag -> msg
    , tryChange : Tag -> { newValue : String } -> msg
    , tryRestore : TagToRemove -> { newValue : String } -> msg
    , tryCreate : { newName : String, newValue : String } -> msg
    , markInProgress : TagInProgress -> msg
    }


viewToRemove : Handlers msg -> Int -> TagToRemove -> Html msg
viewToRemove { markInProgress } idx ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfRemoved ttr
        , Html.button
            [ Evt.onClick <| markInProgress <| Restoring idx { newValue = "" } ]
            [ Html.text "(Restore)" ]
        ]


viewRestoring : Handlers msg -> Int -> String -> TagToRemove -> Html msg
viewRestoring handlers idx newValue ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfRemoved ttr
        , Html.input
            [ Evt.onInput <| \str -> handlers.markInProgress <| Restoring idx { newValue = str }
            , Evt.onSubmit <| handlers.tryRestore ttr { newValue = newValue }
            , Attrs.class "border-black border-solid border-2"
            ]
            [ Html.text newValue ]
        , Html.button
            [ Evt.onClick <| handlers.tryRestore ttr { newValue = newValue } ]
            [ Html.text "(Update)" ]
        ]


viewChangingTag : Handlers msg -> Int -> String -> Tag -> Html msg
viewChangingTag handlers idx newValue tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag
        , Html.input
            [ Evt.onInput <| \str -> handlers.markInProgress <| Changing idx { newValue = str }
            , Evt.onSubmit <| handlers.tryChange tag { newValue = newValue }
            , Attrs.class "border-black border-solid border-2"
            ]
            [ Html.text newValue ]
        , Html.button
            [ Evt.onClick <| handlers.tryChange tag { newValue = newValue } ]
            [ Html.text "(Update)" ]
        ]


viewTag : Handlers msg -> Int -> Tag -> Html msg
viewTag { tryRemove, markInProgress } idx tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag ++ " : " ++ Tag.valueOf tag
        , Html.button
            [ Evt.onClick <| markInProgress <| Changing idx { newValue = Tag.valueOf tag }
            ]
            [ Html.text "(Update value)" ]
        , Html.button
            [ Evt.onClick <| tryRemove tag ]
            [ Html.text "(Remove)" ]
        ]


view : Handlers msg -> Maybe TagInProgress -> List (Either TagToRemove Tag) -> Html msg
view handlers mbInProgress items =
    Html.ul
        [ ]
        <| List.indexedMap
            (\idx ->
                Either.unpack
                        (case mbInProgress of
                            Just (Restoring otherIdx { newValue }) ->
                                if (otherIdx == idx)
                                    then viewRestoring handlers idx newValue
                                    else viewToRemove handlers idx
                            _ ->
                                viewToRemove handlers idx
                        )
                        (case mbInProgress of
                            Just (Changing otherIdx { newValue }) ->
                                if (otherIdx == idx)
                                    then viewChangingTag handlers idx newValue
                                    else viewTag handlers idx
                            _ ->
                                viewTag handlers idx
                        )
            ) items
        ++
        [ case mbInProgress of
            Just (Creating { newName, newValue }) ->
                Html.div
                        []
                        [ Html.input
                            [ Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = str, newValue = "" }
                            , Evt.onSubmit <| handlers.tryCreate { newName = newName, newValue = newValue }
                            , Attrs.class "border-black border-solid border-2"
                            ]
                            [ Html.text newName ]
                        , Html.input
                            [ Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = newName, newValue = str }
                            , Evt.onSubmit <| handlers.tryCreate { newName = newName, newValue = newValue }
                            , Attrs.class "border-black border-solid border-2"
                            ]
                            [ Html.text newValue ]
                        , Html.button
                            [ Evt.onClick <| handlers.tryCreate { newName = newName, newValue = newValue }
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