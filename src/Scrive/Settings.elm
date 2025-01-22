module Scrive.Settings exposing (..)


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI


type DataRetentionPolicy
    = Preparation
    | Closed
    | Canceled
    | TimedOut
    | Rejected
    | Error


type alias PolicyList = List { policy : DataRetentionPolicy, value : Int }


type alias PolicyRec =
    { preparation : Maybe Int
    , closed : Maybe Int
    , canceled : Maybe Int
    , timedout : Maybe Int
    , rejected : Maybe Int
    , error : Maybe Int
    , trash : Maybe ImmediateTrash
    }


type alias Settings =
    { inheritedFrom : NullableInt
    , policy : PolicyRec
    }


type ImmediateTrash = ImmediateTrash


emptyRec : PolicyRec
emptyRec =
    { preparation = Nothing
    , closed = Nothing
    , canceled = Nothing
    , timedout = Nothing
    , rejected = Nothing
    , error = Nothing
    , trash = Nothing
    }


fromList : PolicyList -> PolicyRec
fromList =
    let
        foldItem : { policy : DataRetentionPolicy, value : Int } -> PolicyRec -> PolicyRec
        foldItem { policy, value } rec =
            { preparation = case policy of
                Preparation -> Just value
                _ -> rec.preparation
            , closed = case policy of
                Closed -> Just value
                _ -> rec.closed
            , canceled = case policy of
                Canceled -> Just value
                _ -> rec.canceled
            , timedout = case policy of
                TimedOut -> Just value
                _ -> rec.timedout
            , rejected = case policy of
                Rejected -> Just value
                _ -> rec.rejected
            , error = case policy of
                Error -> Just value
                _ -> rec.error
            , trash = rec.trash
            }
    in List.foldl foldItem emptyRec


toList : PolicyRec -> PolicyList
toList rec =
    let
        toItem : DataRetentionPolicy -> Maybe Int -> Maybe { policy : DataRetentionPolicy, value : Int }
        toItem policy = Maybe.map (\v -> { policy = policy, value = v })
    in
        [ rec.preparation |> toItem Preparation
        , rec.closed      |> toItem Closed
        , rec.canceled    |> toItem Canceled
        , rec.timedout    |> toItem TimedOut
        , rec.rejected    |> toItem Rejected
        , rec.error       |> toItem Error
        ] |> List.filterMap identity




policyDecoder : Decoder PolicyRec
policyDecoder =
    let
      decodeTrash v = if v then Just ImmediateTrash else Nothing
    in
    D.map7 PolicyRec -- NB: field order should match the same in the record type & API
        (D.field "idle_doc_timeout_preparation" <| D.nullable D.int)
        (D.field "idle_doc_timeout_closed"      <| D.nullable D.int)
        (D.field "idle_doc_timeout_canceled"    <| D.nullable D.int)
        (D.field "idle_doc_timeout_timedout"    <| D.nullable D.int)
        (D.field "idle_doc_timeout_rejected"    <| D.nullable D.int)
        (D.field "idle_doc_timeout_error"       <| D.nullable D.int)
        (D.field "immediate_trash" <| D.map decodeTrash <| D.bool)


decoder : Decoder Settings
decoder =
    D.map2
        Settings
        (D.field "inherited_from" NI.decode)
        (D.field "data_retention_policy" policyDecoder)


encodePolicy : PolicyRec -> E.Value
encodePolicy rec =
    let
        encodeField = Maybe.map E.int >> Maybe.withDefault E.null
    in E.object
        [ ( "idle_doc_timeout_preparation", encodeField rec.preparation )
        , ( "idle_doc_timeout_closed",      encodeField rec.closed )
        , ( "idle_doc_timeout_canceled",    encodeField rec.canceled )
        , ( "idle_doc_timeout_timedout",    encodeField rec.timedout )
        , ( "idle_doc_timeout_rejected",    encodeField rec.rejected )
        , ( "idle_doc_timeout_error",       encodeField rec.error )
        ,
            ( "immediate_trash"
            , E.bool <| case rec.trash of
                Just _ -> True
                Nothing -> False
            )
        ]


encode : Settings -> E.Value
encode rec =
    E.object
        [ ( "inherited_from", NI.encode rec.inheritedFrom )
        , ( "data_retention_policy", encodePolicy rec.policy )
        ]


retentionPolicyToString : DataRetentionPolicy -> String
retentionPolicyToString drp =
    case drp of
        Preparation -> "Preparation"
        Closed -> "Closed"
        Canceled -> "Canceled"
        TimedOut -> "Timed out"
        Rejected -> "Rejected"
        Error -> "Error"