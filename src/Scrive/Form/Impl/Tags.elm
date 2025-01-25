module Scrive.Form.Impl.Tags exposing (TagInProgress, indexOfTagInProgress, tagInProgressToString, view)

import Either as Either exposing (Either(..))
import Html as Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Evts
import Html.Events.Extra as Evts
import Scrive.Data.Tag as Tag exposing (ArchivedTag, SomeTag, Tag)
import Scrive.Data.UserGroup as UG
import Scrive.Form.Error as Errors exposing (Error)
import Scrive.Form.Field as Field exposing (Field)
import Style as Style


type TagInProgress
    = Changing Int { newValue : String }
    | Restoring Int { newValue : String }
    | Creating { newName : String, newValue : String }


type alias Handlers msg =
    { archive : Tag -> msg
    , tryChange : Tag -> { newValue : String } -> msg
    , tryRestore : ArchivedTag -> { newValue : String } -> msg
    , tryCreate : { newName : String, newValue : String } -> msg
    , remove : SomeTag -> msg
    , setInProgress : TagInProgress -> msg
    }


view : List Error -> Handlers msg -> Maybe TagInProgress -> List SomeTag -> Html msg
view errors handlers tagInProgress items =
    let
        viewListItem idx =
            Either.unpack
                -- if it's a removed tag, we could either restoring it or just viewing it
                (case tagInProgress of
                    Just (Restoring otherIdx { newValue }) ->
                        if otherIdx == idx then
                            viewWhileRestoringTag errors handlers idx newValue

                        else
                            viewArchivedTag handlers idx

                    _ ->
                        viewArchivedTag handlers idx
                )
                -- if it's a "normal" tag, we could either updating its value or just viewing it
                (case tagInProgress of
                    Just (Changing otherIdx { newValue }) ->
                        if otherIdx == idx then
                            viewWhileChangingTag errors handlers idx newValue

                        else
                            viewTag handlers idx

                    _ ->
                        viewTag handlers idx
                )

        addTagButton =
            Html.button
                [ Evts.onClick <| handlers.setInProgress <| Creating { newName = "", newValue = "" }
                , Attrs.title <| Style.altText Style.AddNewTag
                , Attrs.class Style.button
                ]
                [ Html.text <| Style.buttonLabel Style.AddNewTag ]
    in
    Html.div
        []
    <|
        (Html.ul [ Attrs.class Style.tagList ] <| List.indexedMap viewListItem items)
            :: (case tagInProgress of
                    Just (Creating newData) ->
                        [ viewWhileCreatingTag errors handlers newData ]

                    Just (Restoring _ _) ->
                        []

                    Just (Changing _ _) ->
                        []

                    Nothing ->
                        [ addTagButton ]
               )


viewTag :
    { r
        | setInProgress : TagInProgress -> msg
        , archive : Tag -> msg
        , remove : SomeTag -> msg
    }
    -> Int
    -> Tag
    -> Html msg
viewTag handlers idx tag =
    Html.li
        [ Attrs.class Style.tagItem ]
        [ Html.span [ Attrs.class Style.tagName ] [ Html.text <| Tag.nameOf tag ]
        , Html.span [ Attrs.class Style.tagValue ] [ Html.text <| Tag.valueOf tag ]
        , Html.button
            [ Evts.onClick <| handlers.setInProgress <| Changing idx { newValue = Tag.valueOf tag }
            , Attrs.title <| Style.altText Style.UpdateTagValue
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.UpdateTagValue ]
        , Html.button
            [ Evts.onClick <| handlers.archive tag
            , Attrs.title <| Style.altText Style.ArchiveTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.ArchiveTag ]
        , Html.button
            [ Evts.onClick <| handlers.remove <| Right tag
            , Attrs.title <| Style.altText Style.RemoveTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.RemoveTag ]
        ]


viewArchivedTag :
    { r
        | setInProgress : TagInProgress -> msg
        , remove : SomeTag -> msg
    }
    -> Int
    -> ArchivedTag
    -> Html msg
viewArchivedTag handlers idx archivedTag =
    Html.li
        [ Attrs.class Style.archivedTagItem ]
        [ Html.span [ Attrs.class Style.archivedTagName ] [ Html.text <| Tag.nameOfArchived archivedTag ]
        , Html.button
            [ Evts.onClick <| handlers.setInProgress <| Restoring idx { newValue = "" }
            , Attrs.title <| Style.altText Style.RestoreTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.RestoreTag ]
        , Html.button
            [ Evts.onClick <| handlers.remove <| Left archivedTag
            , Attrs.title <| Style.altText Style.RemoveTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.RemoveTag ]
        ]


viewWhileCreatingTag :
    List Error
    ->
        { r
            | setInProgress : TagInProgress -> msg
            , tryCreate : { newName : String, newValue : String } -> msg
        }
    -> { newName : String, newValue : String }
    -> Html msg
viewWhileCreatingTag errors handlers { newName, newValue } =
    Html.li
        [ Attrs.class Style.editingTagItem ]
        [ Html.input
            [ Attrs.type_ "text"
            , Attrs.class Style.textInput
            , Evts.onInput <| \str -> handlers.setInProgress <| Creating { newName = str, newValue = newValue }
            , Evts.onEnter <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text newName ]
        , Errors.viewMany <| Errors.extractOnlyAt Field.NewTagName errors
        , Html.input
            [ Attrs.type_ "text"
            , Attrs.class Style.textInput
            , Evts.onInput <| \str -> handlers.setInProgress <| Creating { newName = newName, newValue = str }
            , Evts.onEnter <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt Field.NewTagValue errors
        , Html.button
            [ Evts.onClick <| handlers.tryCreate { newName = newName, newValue = newValue }
            , Attrs.title <| Style.altText Style.SubmitNewTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.SubmitNewTag ]
        ]


viewWhileChangingTag :
    List Error
    ->
        { r
            | setInProgress : TagInProgress -> msg
            , tryChange : Tag -> { newValue : String } -> msg
        }
    -> Int
    -> String
    -> Tag
    -> Html msg
viewWhileChangingTag errors handlers idx newValue tag =
    Html.li
        [ Attrs.class Style.editingTagItem ]
        [ Html.span [ Attrs.class Style.tagName ] [ Html.text <| Tag.nameOf tag ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.NameOfTag idx) errors
        , Html.input
            [ Attrs.class Style.textInput
            , Attrs.placeholder <| Tag.valueOf tag
            , Evts.onInput <| \str -> handlers.setInProgress <| Changing idx { newValue = str }
            , Evts.onEnter <| handlers.tryChange tag { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTag idx) errors
        , Html.button
            [ Evts.onClick <| handlers.tryChange tag { newValue = newValue }
            , Attrs.title <| Style.altText Style.SubmitTagValue
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.SubmitTagValue ]
        ]


viewWhileRestoringTag :
    List Error
    ->
        { r
            | setInProgress : TagInProgress -> msg
            , tryRestore : ArchivedTag -> { newValue : String } -> msg
        }
    -> Int
    -> String
    -> ArchivedTag
    -> Html msg
viewWhileRestoringTag errors handlers idx newValue archivedTag =
    Html.li
        [ Attrs.class Style.editingTagItem ]
        [ Html.span [ Attrs.class Style.archivedTagName ] [ Html.text <| Tag.nameOfArchived archivedTag ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.NameOfTag idx) errors
        , Html.input
            [ Attrs.class Style.textInput
            , Attrs.placeholder <|
                case Tag.lastValueOfArchived archivedTag of
                    Just lastValue ->
                        lastValue

                    Nothing ->
                        ""
            , Evts.onInput <| \str -> handlers.setInProgress <| Restoring idx { newValue = str }
            , Evts.onEnter <| handlers.tryRestore archivedTag { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTag idx) errors
        , Html.button
            [ Evts.onClick <| handlers.tryRestore archivedTag { newValue = newValue }
            , Attrs.title <| Style.altText Style.SubmitValueForRestoredTag
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.SubmitValueForRestoredTag ]
        ]


indexOfTagInProgress : TagInProgress -> Maybe Int
indexOfTagInProgress tip =
    case tip of
        Changing n _ -> Just n
        Restoring n _ -> Just n
        Creating _ -> Nothing


tagInProgressToString : TagInProgress -> String
tagInProgressToString tip =
    case tip of
        Changing n { newValue } ->
            "Changing tag #" ++ String.fromInt n ++ " (" ++ newValue ++ ")"

        Restoring n { newValue } ->
            "Restoring tag #" ++ String.fromInt n ++ " (" ++ newValue ++ ")"

        Creating { newName, newValue } ->
            "Creating tag " ++ " (" ++ newName ++ " : " ++ newValue ++ ")"
