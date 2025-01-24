module Scrive.Form.Impl.Tags exposing (TagInProgress, view, indexOfTagInProgress, tagInProgressToString)

import Either exposing (Either(..))
import Either as Either

import Html exposing (Html)
import Html as Html
import Html.Attributes as Attrs
import Html.Events as Evt

import Scrive.Data.Tag exposing (Tag, ArchivedTag, SomeTag)
import Scrive.Data.Tag as Tag
import Scrive.Data.UserGroup as UG
import Scrive.Form.Field exposing (Field)
import Scrive.Form.Field as Field

import Scrive.Form.Error exposing (Error)
import Scrive.Form.Error as Errors


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
                        if (otherIdx == idx)
                            then viewWhileRestoringTag errors handlers idx newValue
                            else viewArchivedTag handlers idx
                    _ ->
                        viewArchivedTag handlers idx
                )

                -- if it's a "normal" tag, we could either updating its value or just viewing it
                (case tagInProgress of
                    Just (Changing otherIdx { newValue }) ->
                        if (otherIdx == idx)
                            then viewWhileChangingTag errors handlers idx newValue
                            else viewTag handlers idx
                    _ ->
                        viewTag handlers idx
                )

        addTagButton =
            Html.button
                [ Evt.onClick <| handlers.setInProgress <| Creating { newName = "", newValue = "" } ]
                [ Html.text "(Add)" ]

    in Html.div
        []
        <|
            ( Html.ul [ ] <| List.indexedMap viewListItem items )
            ::
            case tagInProgress of
                Just (Creating newData) ->
                    [ viewWhileCreatingTag errors handlers newData ]
                Just (Restoring _ _ )->
                    [ ]
                Just (Changing _ _) ->
                    [ ]
                Nothing ->
                    [ addTagButton ]



viewTag :
    { r
        | setInProgress : TagInProgress -> msg
        , archive : Tag -> msg
        , remove : SomeTag -> msg
    }
    -> Int -> Tag -> Html msg
viewTag handlers idx tag =
    Html.li
        []
        [ Html.text <| Tag.nameOf tag ++ " : " ++ Tag.valueOf tag
        , Html.button
            [ Evt.onClick <| handlers.setInProgress <| Changing idx { newValue = Tag.valueOf tag }
            ]
            [ Html.text "(Update value)" ]
        , Html.button
            [ Evt.onClick <| handlers.archive tag ]
            [ Html.text "(Archive)" ]
        , Html.button
            [ Evt.onClick <| handlers.remove <| Right tag ]
            [ Html.text "(Remove)" ]
        ]


viewArchivedTag :
    { r
        | setInProgress : TagInProgress -> msg
        , remove : SomeTag -> msg
    }
    -> Int -> ArchivedTag -> Html msg
viewArchivedTag handlers idx ttr =
    Html.li
        []
        [ Html.text <| Tag.nameOfArchived ttr
        , Html.button
            [ Evt.onClick <| handlers.setInProgress <| Restoring idx { newValue = "" } ]
            [ Html.text "(Restore)" ]
        , Html.button
            [ Evt.onClick <| handlers.remove <| Left ttr ]
            [ Html.text "(Remove)" ]
        ]


viewWhileCreatingTag :
    List Error ->
    { r
        | setInProgress : TagInProgress -> msg
        , tryCreate : { newName : String, newValue : String } -> msg
    }
    -> { newName : String, newValue : String } -> Html msg
viewWhileCreatingTag errors handlers { newName, newValue } =
    Html.li
        []
        [ Html.input
            [ Attrs.type_ "text"
            , Attrs.class "border-black border-solid border-2"
            , Evt.onInput <| \str -> handlers.setInProgress <| Creating { newName = str, newValue = newValue }
            , Evt.onSubmit <| handlers.tryCreate { newName = newName, newValue = newValue }
            ]
            [ Html.text newName ]
        , Errors.viewMany <| Errors.extractOnlyAt Field.NewTagName errors
        , Html.input
            [ Attrs.type_ "text"
            , Attrs.class "border-black border-solid border-2"
            , Evt.onInput <| \str -> handlers.setInProgress <| Creating { newName = newName, newValue = str }
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
        | setInProgress : TagInProgress -> msg
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
            , Evt.onInput <| \str -> handlers.setInProgress <| Changing idx { newValue = str }
            , Evt.onSubmit <| handlers.tryChange tag { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTag idx) errors
        , Html.button
            [ Evt.onClick <| handlers.tryChange tag { newValue = newValue } ]
            [ Html.text "(Update)" ]
        ]


viewWhileRestoringTag :
    List Error ->
    { r
        | setInProgress : TagInProgress -> msg
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
            , Evt.onInput <| \str -> handlers.setInProgress <| Restoring idx { newValue = str }
            , Evt.onSubmit <| handlers.tryRestore ttr { newValue = newValue }
            ]
            [ Html.text newValue ]
        , Errors.viewMany <| Errors.extractOnlyAt (Field.ValueOfTag idx) errors
        , Html.button
            [ Evt.onClick <| handlers.tryRestore ttr { newValue = newValue } ]
            [ Html.text "(Set)" ]
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
        Changing n { newValue } -> "Changing tag #" ++ String.fromInt n ++ " (" ++ newValue ++ ")"
        Restoring n { newValue }  -> "Restoring tag #" ++ String.fromInt n ++ " (" ++ newValue ++ ")"
        Creating { newName, newValue } -> "Creating tag " ++ " (" ++ newName ++ " : " ++ newValue ++ ")"