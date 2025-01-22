module DataRetentionPolicy exposing (DataRetentionPolicy, decoder)

import Json.Decode exposing (Decoder)


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


decoder : Decoder DataRetentionPolicy
decoder =
    Json.Decode.map7 DataRetentionPolicy
        (Json.Decode.field "idle_doc_timeout_preparation" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "idle_doc_timeout_closed" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "idle_doc_timeout_canceled" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "idle_doc_timeout_timedout" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "idle_doc_timeout_rejected" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "idle_doc_timeout_error" (Json.Decode.maybe Json.Decode.int))
        (Json.Decode.field "immediate_trash" Json.Decode.bool)
