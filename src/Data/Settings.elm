module Data.Settings exposing (Model, decode)

import Data.InheritedFrom as InheritedFrom
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type Model
    = Model Settings


type alias Settings =
    { inheritedFrom : InheritedFrom.Model
    , dataRetentionPolicy : DataRetentionPolicy
    }


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


decode : Decoder Model
decode =
    Decode.map Model decodeSettings


decodeSettings : Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> Pipeline.required "inherited_from" InheritedFrom.decode
        |> Pipeline.required "data_retention_policy" decodeDataRetentionPolicy


decodeDataRetentionPolicy : Decoder DataRetentionPolicy
decodeDataRetentionPolicy =
    Decode.succeed DataRetentionPolicy
        |> Pipeline.required "idle_doc_timeout_preparation" decodeNullableInt
        |> Pipeline.required "idle_doc_timeout_closed" decodeNullableInt
        |> Pipeline.required "idle_doc_timeout_canceled" decodeNullableInt
        |> Pipeline.required "idle_doc_timeout_timedout" decodeNullableInt
        |> Pipeline.required "idle_doc_timeout_rejected" decodeNullableInt
        |> Pipeline.required "idle_doc_timeout_error" decodeNullableInt
        |> Pipeline.required "immediate_trash" Decode.bool


decodeNullableInt : Decoder (Maybe Int)
decodeNullableInt =
    Decode.nullable Decode.int
