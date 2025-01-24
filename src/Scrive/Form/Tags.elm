module Scrive.Form.Tags exposing (TagInProgress, view, none)

import Either exposing (Either(..))
import Either as Either

import Html exposing (Html)
import Html as Html
import Html.Attributes as Attrs
import Html.Events as Evt

import Scrive.Tag exposing (Tag, ArchivedTag, SomeTag)
import Scrive.Tag as Tag
import Scrive.UserGroup as UG
import Scrive.Form.Field exposing (Field)
import Scrive.Form.Field as Field

import Scrive.Form.Error exposing (Error)
import Scrive.Form.Error as Errors


type TagInProgress -- constructors are not exposed, except `None`
    = Changing Int { newValue : String }
    | Restoring Int { newValue : String }
    | Creating { newName : String, newValue : String }
    | None


type alias Handlers msg =
    { tryArchive : Tag -> msg
    , tryChange : Tag -> { newValue : String } -> msg
    , tryRestore : ArchivedTag -> { newValue : String } -> msg
    , tryCreate : { newName : String, newValue : String } -> msg
    , remove : SomeTag -> msg
    , markInProgress : TagInProgress -> msg
    }


none : TagInProgress
none = None


view : List Error -> Handlers msg -> TagInProgress -> List SomeTag -> Html msg
view errors handlers tagInProgress items =
    let

        viewListItem idx =
            Either.unpack

                -- if it's a removed tag, we could either restoring it or just viewing it
                (case tagInProgress of
                    Restoring otherIdx { newValue } ->
                        if (otherIdx == idx)
                            then viewWhileRestoringTag errors handlers idx newValue
                            else viewArchivedTag handlers idx
                    _ ->
                        viewArchivedTag handlers idx
                )

                -- if it's a "normal" tag, we could either updating its value or just viewing it
                (case tagInProgress of
                    Changing otherIdx { newValue } ->
                        if (otherIdx == idx)
                            then viewWhileChangingTag errors handlers idx newValue
                            else viewTag handlers idx
                    _ ->
                        viewTag handlers idx
                )

        addTagButton =
            Html.button
                [ Evt.onClick <| handlers.markInProgress <| Creating { newName = "", newValue = "" } ]
                [ Html.text "(Add)" ]

    in Html.div
        []
        <|
            ( Html.ul [ ] <| List.indexedMap viewListItem items )
            ::
            case tagInProgress of
                Creating newData ->
                    [ viewWhileCreatingTag errors handlers newData ]
                Restoring _ _ ->
                    [ ]
                Changing _ _ ->
                    [ ]
                None ->
                    [ addTagButton ]



viewTag :
    { r
        | markInProgress : TagInProgress -> msg
        , tryArchive : Tag -> msg
        , remove : SomeTag -> msg
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
            [ Evt.onClick <| handlers.tryArchive tag ]
            [ Html.text "(Archive)" ]
        , Html.button
            [ Evt.onClick <| handlers.remove <| Right tag ]
            [ Html.text "(Remove)" ]
        ]


viewArchivedTag :
    { r
        | markInProgress : TagInProgress -> msg
        , remove : SomeTag -> msg
    }
    -> Int -> ArchivedTag -> Html msg
viewArchivedTag handlers idx ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfArchived ttr
        , Html.button
            [ Evt.onClick <| handlers.markInProgress <| Restoring idx { newValue = "" } ]
            [ Html.text "(Restore)" ]
        , Html.button
            [ Evt.onClick <| handlers.remove <| Left ttr ]
            [ Html.text "(Remove)" ]
        ]


viewWhileCreatingTag :
    List Error ->
    { r
        | markInProgress : TagInProgress -> msg
        , tryCreate : { newName : String, newValue : String } -> msg
    }
    -> { newName : String, newValue : String } -> Html msg
viewWhileCreatingTag errors handlers { newName, newValue } =
    Html.li
        []
        [ Html.input
            [ Attrs.type_ "text"
            , Attrs.class "border-black border-solid border-2"
            , Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = str, newValue = newValue }
            , Evt.onSubmit <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text newName ]
        , Errors.viewMany <| Errors.extractOnlyAt Field.NewTagName errors
        , Html.input
            [ Attrs.type_ "text"
            , Attrs.class "border-black border-solid border-2"
            , Evt.onInput <| \str -> handlers.markInProgress <| Creating { newName = newName, newValue = str }
            , Evt.onSubmit <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt Field.NewTagValue errors
        , Html.button
            [ Evt.onClick <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text "(Submit)" ]
        ]


viewWhileChangingTag :
    List Error ->
    { r
        | markInProgress : TagInProgress -> msg
        , tryChange : Tag -> { newValue : String } -> msg
    }
    -> Int -> String -> Tag -> Html msg
viewWhileChangingTag errors handlers idx newValue tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag
        , Html.input
            [ Attrs.class "border-black border-solid border-2"
            , Attrs.placeholder <| Tag.valueOf tag
            , Evt.onInput <| \str -> handlers.markInProgress <| Changing idx { newValue = str }
            , Evt.onSubmit <| handlers.tryChange tag { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTagWith { name = Tag.nameOf tag }) errors
        , Html.button
            [ Evt.onClick <| handlers.tryChange tag { newValue = newValue } ]
            [ Html.text "(Update)" ]
        ]


viewWhileRestoringTag :
    List Error ->
    { r
        | markInProgress : TagInProgress -> msg
        , tryRestore : ArchivedTag -> { newValue : String } -> msg
    }
    -> Int -> String -> ArchivedTag -> Html msg
viewWhileRestoringTag errors handlers idx newValue ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfArchived ttr
        , Html.input
            [ Attrs.placeholder <| case Tag.lastValueOfArchived ttr of
                Just lastValue -> lastValue
                Nothing -> ""
            , Attrs.class "border-black border-solid border-2"
            , Evt.onInput <| \str -> handlers.markInProgress <| Restoring idx { newValue = str }
            , Evt.onSubmit <| handlers.tryRestore ttr { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTagWith { name = Tag.nameOfArchived ttr }) errors
        , Html.button
            [ Evt.onClick <| handlers.tryRestore ttr { newValue = newValue } ]
            [ Html.text "(Set)" ]
        ]
