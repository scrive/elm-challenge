module Form.RetentionPolicy exposing (..)


import Html exposing (Html)
import Html
import Html.Attributes as Attrs
import Html.Events as Evts

import Scrive.Settings exposing (DataRetentionPolicy, PolicyWithTimeout)
import Scrive.Settings as S

type alias Handlers msg =
    { starDefiningPolicy : DataRetentionPolicy -> msg
    , tryDefineTimeout : DataRetentionPolicy -> Int -> msg
    , clearTimeout : DataRetentionPolicy -> msg
    }


view : Handlers msg -> Maybe DataRetentionPolicy -> List PolicyWithTimeout -> Html msg
view handlers mbCurrentPolicy pl =
    let
        isCurrent policy = case mbCurrentPolicy of
            Just currentPolicy -> currentPolicy == policy
            Nothing -> False
        viewPolicy { policy, value } =
            Html.li []
                [ Html.text <| S.retentionPolicyToString policy ++ " timeout : "
                , if not <| isCurrent policy
                    then
                        Html.div []
                            [ Html.text <| String.fromInt value
                            , Html.button
                                [ Evts.onClick <| handlers.starDefiningPolicy policy
                                ]
                                [ Html.text "(Update)" ]
                            ]
                    else
                        Html.div []
                            [ Html.input
                                [ Attrs.type_ "number", Attrs.value "0"
                                , Attrs.placeholder <| String.fromInt value
                                , Evts.onInput (\str -> Maybe.withDefault (handlers.clearTimeout policy) <| Maybe.map (handlers.tryDefineTimeout policy) <| String.toInt str)
                                ]
                                []
                            , Html.button [] [ Html.text "(Set)" ]
                            ]
                ]
        viewLackingPolicy policy =
            Html.li []
                [ Html.text <| S.retentionPolicyToString policy ++ " timeout : -"
                , if not <| isCurrent policy
                    then
                        Html.div []
                            [ Html.text "-"
                            , Html.button
                                [ Evts.onClick <| handlers.starDefiningPolicy policy
                                ]
                                [ Html.text "(Define)"
                                ]
                            ]
                    else
                        Html.div []
                            [ Html.input
                                [ Attrs.type_ "number", Attrs.value "0"
                                , Evts.onInput (\str -> Maybe.withDefault (handlers.clearTimeout policy) <| Maybe.map (handlers.tryDefineTimeout policy) <| String.toInt str)
                                ]
                                []
                            , Html.button [] [ Html.text "(Set)" ]
                            ]
                ]
    in
        Html.div
            []
            [ Html.ul [] <| List.map viewPolicy pl
            , Html.ul [] <| List.map viewLackingPolicy <| S.lacksWhichPolicies pl
            , Html.input [ Attrs.type_ "checkbox" ] [ Html.text "Immediate trash (no timeouts)" ]
            ]
