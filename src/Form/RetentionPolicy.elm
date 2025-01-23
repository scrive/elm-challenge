module Form.RetentionPolicy exposing (..)


import Html exposing (Html)
import Html
import Html.Attributes as Attrs
import Html.Events as Evts

import Scrive.RetentionPolicy exposing (DataRetentionPolicy, PolicyWithTimeout)
import Scrive.RetentionPolicy as RP


type alias Handlers msg =
    { startDefiningPolicy : DataRetentionPolicy -> msg
    , tryDefineTimeout : DataRetentionPolicy -> Int -> msg
    , applyCurrentTimeout : DataRetentionPolicy -> msg
    , clearTimeout : DataRetentionPolicy -> msg
    , selectPolicyToAdd : DataRetentionPolicy -> msg
    , addSelectedPolicy : msg
    , toggleImmediateTrash : Bool -> msg
    }


view : Handlers msg -> Maybe DataRetentionPolicy -> List PolicyWithTimeout -> Html msg
view handlers mbCurrentPolicy pl =
    let
        isCurrent policy = case mbCurrentPolicy of
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
                                , Evts.onInput (\str -> Maybe.withDefault (handlers.clearTimeout policy) <| Maybe.map (handlers.tryDefineTimeout policy) <| String.toInt str)
                                ]
                                []
                            , Html.button
                                [ Evts.onClick <| handlers.applyCurrentTimeout policy ]
                                [ Html.text "(Set)" ]
                            ]
                ]
        viewLackingPolicyOption policy =
            Html.option []
                [ Html.text <| RP.toString policy ++ " timeout" ]
        lacksWhichPolicies = RP.lacksWhichPolicies pl
    in
        Html.div
            []
            [ Html.ul [] <| List.map viewPolicy pl

            , if List.length lacksWhichPolicies > 0 then
                 Html.select
                    [ Evts.onInput
                        (handlers.selectPolicyToAdd << RP.fromOption)
                    ] <| List.map viewLackingPolicyOption lacksWhichPolicies
              else Html.text ""

            , Html.button
                [ Evts.onClick handlers.addSelectedPolicy
                ]
                [ Html.text "(Add)" ]

            , Html.input
                [ Attrs.type_ "checkbox"
                , Evts.onCheck handlers.toggleImmediateTrash
                ]
                [ Html.text "Immediate trash (no timeouts)"
                ]
            ]
