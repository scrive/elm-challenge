module Scrive.Form.Impl.RetentionPolicy exposing (..)


import Html exposing (Html)
import Html
import Html.Extra as Html
import Html.Attributes as Attrs
import Html.Events as Evts
import Html.Events.Extra as Evts

import Style as Style

import Scrive.Data.RetentionPolicy exposing (DataRetentionPolicy, PolicyWithTimeout)
import Scrive.Data.RetentionPolicy as RP
import Scrive.Form.Error exposing (Error)
import Scrive.Form.Error as Errors


type alias Handlers msg =
    { editCurrentTimeout : DataRetentionPolicy -> Int -> msg
    , applyCurrentTimeout : DataRetentionPolicy -> msg
    , clearTimeout : DataRetentionPolicy -> msg
    , selectPolicyToAdd : DataRetentionPolicy -> msg
    , clearPolicyToAdd : msg
    , addSelectedPolicy : msg
    , toggleImmediateTrash : Bool -> msg
    }


type alias State =
    { currentlyEditing : Maybe ( DataRetentionPolicy, Int )
    , currentlyAdding : Maybe DataRetentionPolicy
    }


view : List Error -> Handlers msg -> State -> List PolicyWithTimeout -> Html msg
view errors handlers state pl =
    let
        lacksWhichPolicies = RP.lacksWhichPolicies pl

        mbCurrentlyEdited policy =
            state.currentlyEditing
                |> Maybe.andThen
                    (\( currentPolicy, currentVal ) ->
                        if currentPolicy == policy then
                            -- only returns `Just` when the same policy as in argument is being edited
                            Just ( currentPolicy, currentVal )
                        else Nothing
                    )

        viewPolicy { policy, value } =
            Html.li []
                [ Html.span [ Attrs.class Style.fieldLabel ] [ Html.text <| RP.toOption policy ++ " : " ]
                , case mbCurrentlyEdited policy of
                    Nothing ->
                        Html.span [ Attrs.class Style.itemWithValue ]
                            [ Html.span
                                [ Attrs.class Style.fieldValue ]
                                [ Html.text <| String.fromInt value ]
                            , Html.button
                                [ Evts.onClick <| handlers.editCurrentTimeout policy value
                                , Attrs.title <| Style.altText Style.UpdateTimeout
                                , Attrs.class Style.button
                                ]
                                [ Html.text <| Style.buttonLabel Style.UpdateTimeout ]
                            , Html.button
                                [ Evts.onClick <| handlers.clearTimeout policy
                                , Attrs.title <| Style.altText Style.RemoveTimeout
                                , Attrs.class Style.button
                                ]
                                [ Html.text <| Style.buttonLabel Style.RemoveTimeout ]
                            ]
                    Just ( _, currentValue ) ->
                        Html.span [ Attrs.class Style.itemWithInput ]
                            [ Html.input
                                [ Attrs.type_ "number"
                                , Attrs.class Style.textInput
                                , Attrs.value <| String.fromInt currentValue
                                , Attrs.placeholder <| String.fromInt value
                                , Evts.onInput
                                    ( String.toInt
                                        >> Maybe.withDefault 0
                                        >> handlers.editCurrentTimeout policy
                                    )
                                , Evts.onEnter <| handlers.applyCurrentTimeout policy
                                ]
                                []
                            , Html.button
                                [ Evts.onClick <| handlers.applyCurrentTimeout policy
                                , Attrs.title <| Style.altText Style.SubmitTimeout
                                , Attrs.class Style.button
                                ]
                                [ Html.text <| Style.buttonLabel Style.SubmitTimeout ]
                            ]
                ]

    in
        Html.div
            []
            [ Html.ul [ Attrs.class Style.inputsForValuesList ] <| List.map viewPolicy pl

            , if List.length lacksWhichPolicies > 0 then

                viewAddPolicySelect handlers state lacksWhichPolicies

              else Html.nothing

            , Html.input
                [ Attrs.type_ "checkbox"
                , Attrs.id "retention-immediate-trash"
                , Attrs.class Style.checkBox
                , Evts.onCheck handlers.toggleImmediateTrash
                ]
                [ ]

            , Html.label
                [ Attrs.for "retention-immediate-trash"
                , Attrs.class Style.inputLabel
                ]
                [ Html.text "Immediate trash (no timeouts)" ]

            ]


viewAddPolicySelect : Handlers msg -> State -> List DataRetentionPolicy -> Html msg
viewAddPolicySelect handlers state lacksWhichPolicies =
    let
        viewLackingPolicyOption policy =
            Html.option [ Attrs.class Style.selectOption ]
                [ Html.text <| RP.toString policy ++ " timeout" ]
    in
    Html.div []
        [ Html.select
            [ Evts.onInput
                (\str ->
                    case RP.fromOption str of
                        Just policy -> handlers.selectPolicyToAdd policy
                        Nothing -> handlers.clearPolicyToAdd)
            , Attrs.class Style.selectBox
            ]
            ( Html.option
                [ Attrs.selected <| case state.currentlyAdding of
                    Nothing -> True
                    Just _ -> False
                , Attrs.class Style.selectOption
                ]
                [ Html.text <| Style.textForSelectPolicy ]
            :: List.map viewLackingPolicyOption lacksWhichPolicies
            )
        , Html.button
            [ Attrs.disabled <| case state.currentlyAdding of
                Nothing -> True
                Just policy -> not <| List.member policy lacksWhichPolicies
            , Evts.onClick handlers.addSelectedPolicy
            , Attrs.title <| Style.altText Style.AddPolicy
            , Attrs.class Style.button
            ]
            [ Html.text <| Style.buttonLabel Style.AddPolicy ]
        ]