module Scrive.Form.Impl.RetentionPolicy exposing (..)


import Html exposing (Html)
import Html
import Html.Extra as Html
import Html.Attributes as Attrs
import Html.Events as Evts

import Scrive.Data.RetentionPolicy exposing (DataRetentionPolicy, PolicyWithTimeout)
import Scrive.Data.RetentionPolicy as RP
import Scrive.Form.Error exposing (Error)
import Scrive.Form.Error as Errors


type alias Handlers msg =
    { startDefiningPolicy : DataRetentionPolicy -> msg
    , tryDefineTimeout : DataRetentionPolicy -> Int -> msg
    , applyCurrentTimeout : DataRetentionPolicy -> msg
    , clearTimeout : DataRetentionPolicy -> msg
    , selectPolicyToAdd : DataRetentionPolicy -> msg
    , clearPolicyToAdd : msg
    , addSelectedPolicy : msg
    , toggleImmediateTrash : Bool -> msg
    }


type alias State =
    { editing : Maybe DataRetentionPolicy
    , adding : Maybe DataRetentionPolicy
    }


view : List Error -> Handlers msg -> State -> List PolicyWithTimeout -> Html msg
view errors handlers state pl =
    let
        lacksWhichPolicies = RP.lacksWhichPolicies pl

        isCurrent policy = case state.editing of
            Just currentPolicy -> currentPolicy == policy
            Nothing -> False

        viewPolicy { policy, value } =
            Html.li []
                [ Html.text <| RP.toOption policy ++ " : "
                , if not <| isCurrent policy
                    then
                        Html.div []
                            [ Html.text <| String.fromInt value
                            , Html.button
                                [ Evts.onClick <| handlers.startDefiningPolicy policy
                                ]
                                [ Html.text "(Update)" ]
                            , Html.button
                                [ Evts.onClick <| handlers.clearTimeout policy
                                ]
                                [ Html.text "(Remove)" ]
                            ]
                    else
                        Html.div []
                            [ Html.input
                                [ Attrs.type_ "number", Attrs.value <| String.fromInt value
                                , Attrs.placeholder <| String.fromInt value
                                , Evts.onInput
                                    ( String.toInt
                                        >> Maybe.map (handlers.tryDefineTimeout policy)
                                        >> Maybe.withDefault (handlers.clearTimeout policy)
                                    )
                                ]
                                []
                            , Html.button
                                [ Evts.onClick <| handlers.applyCurrentTimeout policy ]
                                [ Html.text "(Set)" ]
                            ]
                ]

    in
        Html.div
            []
            [ Html.ul [] <| List.map viewPolicy pl

            , if List.length lacksWhichPolicies > 0 then

                viewAddPolicySelect handlers state lacksWhichPolicies

              else Html.nothing

            , Html.input
                [ Attrs.type_ "checkbox"
                , Attrs.id "retention-immediate-trash"
                , Evts.onCheck handlers.toggleImmediateTrash
                ]
                [ ]

            , Html.label
                [ Attrs.for "retention-immediate-trash"
                ]
                [ Html.text "Immediate trash (no timeouts)" ]

            ]


viewAddPolicySelect : Handlers msg -> State -> List DataRetentionPolicy -> Html msg
viewAddPolicySelect handlers state lacksWhichPolicies =
    let
        viewLackingPolicyOption policy =
            Html.option []
                [ Html.text <| RP.toString policy ++ " timeout" ]
    in
    Html.div []
        [ Html.select
            [ Evts.onInput
                (\str ->
                    case RP.fromOption str of
                        Just policy -> handlers.selectPolicyToAdd policy
                        Nothing -> handlers.clearPolicyToAdd)
            ]
            ( Html.option
                [ Attrs.selected <| case state.adding of
                    Nothing -> True
                    Just _ -> False
                ]
                [ Html.text "<Select one>" ]
            :: List.map viewLackingPolicyOption lacksWhichPolicies
            )
        , Html.button
            [ Attrs.disabled <| case state.adding of
                Nothing -> True
                Just policy -> not <| List.member policy lacksWhichPolicies
            , Evts.onClick handlers.addSelectedPolicy
            ]
            [ Html.text "(Add)" ]
        ]