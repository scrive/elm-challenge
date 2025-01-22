module Form.Tags exposing (TagInProgress, view, none)

import Either exposing (Either(..))
import Either as Either

import Scrive.Tag exposing (Tag, TagToRemove)
import Scrive.Tag as Tag

import Html exposing (Html)
import Html as Html
import Html.Attributes as Attrs
import Html.Events as Evt

import Scrive.UserGroup as UG

type TagInProgress -- constructors are not exposed, except `None`
    = Changing Int { newValue : String }
    | Restoring Int { newValue : String }
    | Creating { newName : String, newValue : String }
    | None


type alias Handlers msg =
    { tryRemove : Tag -> msg
    , tryChange : Tag -> { newValue : String } -> msg
    , tryRestore : TagToRemove -> { newValue : String } -> msg
    , tryCreate : { newName : String, newValue : String } -> msg
    , markInProgress : TagInProgress -> msg
    }


none : TagInProgress
none = None


view : Handlers msg -> TagInProgress -> UG.TagList -> Html msg
view handlers tagInProgress items =
    let

        viewListItem idx =
            Either.unpack

                -- if it's a removed tag, we could either restoring it or just viewing it
                (case tagInProgress of
                    Restoring otherIdx { newValue } ->
                        if (otherIdx == idx)
                            then viewWhileRestoringTag handlers idx newValue
                            else viewRemovedTag handlers idx
                    _ ->
                        viewRemovedTag handlers idx
                )

                -- if it's a "normal" tag, we could either updating its value or just viewing it
                (case tagInProgress of
                    Changing otherIdx { newValue } ->
                        if (otherIdx == idx)
                            then viewWhileChangingTag handlers idx newValue
                            else viewTag handlers idx
                    _ ->
                        viewTag handlers idx
                )

        addTagButton =
            Html.button
                [ Evt.onClick <| handlers.markInProgress <| Creating { newName = "", newValue = "" } ]
                [ Html.text "Add" ]

    in Html.div
        []
        <|
            ( Html.ul [ ] <| List.indexedMap viewListItem items )
            ::
            case tagInProgress of
                Creating newData ->
                    [ viewWhileCreatingTag handlers newData ]
                Restoring _ _ ->
                    [ ]
                Changing _ _ ->
                    [ ]
                None ->
                    [ addTagButton ]



viewTag :
    { r
        | markInProgress : TagInProgress -> msg
        , tryRemove : Tag -> msg
    }
    -> Int -> Tag -> Html msg
viewTag handlers idx tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag ++ " : " ++ Tag.valueOf tag
        , Html.button
            [ Evt.onClick <| handlers.markInProgress <| Changing idx { newValue = Tag.valueOf tag }
            ]
            [ Html.text "(Update value)" ]
        , Html.button
            [ Evt.onClick <| handlers.tryRemove tag ]
            [ Html.text "(Remove)" ]
        ]


viewRemovedTag :
    { r
        | markInProgress : TagInProgress -> msg
    }
    -> Int -> TagToRemove -> Html msg
viewRemovedTag handlers idx ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfRemoved ttr
        , Html.button
            [ Evt.onClick <| handlers.markInProgress <| Restoring idx { newValue = "" } ]
            [ Html.text "(Restore)" ]
        ]


viewWhileCreatingTag :
    { r
        | markInProgress : TagInProgress -> msg
        , tryCreate : { newName : String, newValue : String } -> msg
    }
    -> { newName : String, newValue : String } -> Html msg
viewWhileCreatingTag handlers { newName, newValue } =
    Html.li
        []
        [ Html.input
            [ Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = str, newValue = newValue }
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


viewWhileChangingTag :
    { r
        | markInProgress : TagInProgress -> msg
        , tryChange : Tag -> { newValue : String } -> msg
    }
    -> Int -> String -> Tag -> Html msg
viewWhileChangingTag handlers idx newValue tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag
        , Html.input
            [ Evt.onInput <| \str -> handlers.markInProgress <| Changing idx { newValue = str }
            , Evt.onSubmit <| handlers.tryChange tag { newValue = newValue }
            , Attrs.class "border-black border-solid border-2"
            , Attrs.placeholder <| Tag.valueOf tag
            ]
            [ Html.text newValue ]
        , Html.button
            [ Evt.onClick <| handlers.tryChange tag { newValue = newValue } ]
            [ Html.text "(Update)" ]
        ]


viewWhileRestoringTag :
    { r
        | markInProgress : TagInProgress -> msg
        , tryRestore : TagToRemove -> { newValue : String } -> msg
    }
    -> Int -> String -> TagToRemove -> Html msg
viewWhileRestoringTag handlers idx newValue ttr =
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
            [ Html.text "(Set)" ]
        ]
