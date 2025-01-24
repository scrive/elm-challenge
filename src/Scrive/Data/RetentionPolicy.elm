module Scrive.Data.RetentionPolicy exposing
    ( DataRetentionPolicy
    , PolicyWithTimeout
    , PolicyRec
    , possiblePolicies, lacksWhichPolicies
    , setPolicyTimeout, clearPolicyTimeout
    , setImmediateTrash, clearImmediateTrash
    , emptyRec
    , fromList, toList
    , decoder, encode
    , toString, toOption, fromOption
    )


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type DataRetentionPolicy
    = Preparation
    | Closed
    | Canceled
    | TimedOut
    | Rejected
    | Error


possiblePolicies : List DataRetentionPolicy
possiblePolicies = [ Preparation, Closed, Canceled, TimedOut, Rejected, Error ]


type alias PolicyWithTimeout = { policy : DataRetentionPolicy, value : Int }


type alias PolicyRec =
    { preparation : Maybe Int
    , closed : Maybe Int
    , canceled : Maybe Int
    , timedout : Maybe Int
    , rejected : Maybe Int
    , error : Maybe Int
    , trash : Maybe ImmediateTrash
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


fromList : List PolicyWithTimeout -> PolicyRec
fromList =
    let
        foldItem : { policy : DataRetentionPolicy, value : Int } -> PolicyRec -> PolicyRec
        foldItem { policy, value } = changePolicyMbTimeout policy <| Just value
    in List.foldl foldItem emptyRec


toList : PolicyRec -> List PolicyWithTimeout
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



lacksWhichPolicies : List PolicyWithTimeout -> List DataRetentionPolicy
lacksWhichPolicies assignedValues =
    let
        keysOfAssigned =
            List.map .policy assignedValues
    in
        List.foldl
            (\policy lacks -> if List.member policy keysOfAssigned then lacks else policy :: lacks)
            []
            possiblePolicies
        |> List.reverse


-- private
changePolicyMbTimeout : DataRetentionPolicy -> Maybe Int -> PolicyRec -> PolicyRec
changePolicyMbTimeout policy mbTimeout rec =
    { preparation = case policy of
        Preparation -> mbTimeout
        _ -> rec.preparation
    , closed = case policy of
        Closed -> mbTimeout
        _ -> rec.closed
    , canceled = case policy of
        Canceled -> mbTimeout
        _ -> rec.canceled
    , timedout = case policy of
        TimedOut -> mbTimeout
        _ -> rec.timedout
    , rejected = case policy of
        Rejected -> mbTimeout
        _ -> rec.rejected
    , error = case policy of
        Error -> mbTimeout
        _ -> rec.error
    , trash = rec.trash
    }


setPolicyTimeout : DataRetentionPolicy -> Int -> PolicyRec -> PolicyRec
setPolicyTimeout policy = changePolicyMbTimeout policy << Just


clearPolicyTimeout : DataRetentionPolicy -> PolicyRec -> PolicyRec
clearPolicyTimeout policy = changePolicyMbTimeout policy Nothing


setImmediateTrash : PolicyRec -> PolicyRec
setImmediateTrash rec = { rec | trash = Just ImmediateTrash }


clearImmediateTrash : PolicyRec -> PolicyRec
clearImmediateTrash rec = { rec | trash = Nothing }


decoder : Decoder PolicyRec
decoder =
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


encode : PolicyRec -> E.Value
encode rec =
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

toString : DataRetentionPolicy -> String
toString drp =
    case drp of
        Preparation -> "Preparation"
        Closed -> "Closed"
        Canceled -> "Canceled"
        TimedOut -> "Timed out"
        Rejected -> "Rejected"
        Error -> "Error"


toOption : DataRetentionPolicy -> String
toOption drp =
    toString drp ++ " timeout"


-- for forms purpose : should be the exact reverse of `toOption`
fromOption : String -> Maybe DataRetentionPolicy
fromOption str =
    case str of
        "Preparation timeout" -> Just Preparation
        "Closed timeout" -> Just Closed
        "Canceled timeout" -> Just Canceled
        "Timed out timeout" -> Just TimedOut
        "Rejected timeout" -> Just Rejected
        "Error timeout" -> Just Error
        _ -> Nothing -- not very good to return `Error` value here, but it expected never to happen
