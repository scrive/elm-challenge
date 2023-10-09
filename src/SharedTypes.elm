module SharedTypes exposing (..)

import Json.Decode as Decode exposing (Decoder, bool)
import Json.Decode.Pipeline exposing (optional, required)



-- User group record type


type alias UserGroup =
    { settingsData : SettingsData
    , id : String
    , parentId : String
    , name : String
    , children : Maybe (List UserGroupChildren)
    }



-- Define a user group children record type with name and ID fields


type alias UserGroupChildren =
    { name : String
    , id : String
    }



-- Settings record type


type alias SettingsData =
    { inheritedFrom : Maybe String
    , dataRetentionPolicy : DataRetentionPolicy
    }



-- A data retention policy record type with various timeout values


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }



-- Decoder for UserGroup records


userGroupDecoder : Decoder UserGroup
userGroupDecoder =
    Decode.succeed UserGroup
        |> required "settings" settingsDecoder
        |> required "id" Decode.string
        |> required "parent_id" Decode.string
        |> required "name" Decode.string
        |> optional "children" (Decode.maybe (Decode.list userGroupChildrenDecoder)) Nothing



-- decoder for UserGroupChildren records


userGroupChildrenDecoder : Decoder UserGroupChildren
userGroupChildrenDecoder =
    Decode.succeed UserGroupChildren
        |> required "name" Decode.string
        |> required "id" Decode.string



-- decoder for Settings Data records


settingsDecoder : Decoder SettingsData
settingsDecoder =
    Decode.succeed SettingsData
        |> optional "inherited_from" (Decode.maybe Decode.string) Nothing
        |> required "data_retention_policy" dataRetentionPolicyDecoder



-- decoder for DataRetentionPolicy records


dataRetentionPolicyDecoder : Decoder DataRetentionPolicy
dataRetentionPolicyDecoder =
    Decode.succeed DataRetentionPolicy
        |> optional "idle_doc_timeout_preparation" (Decode.maybe Decode.int) Nothing
        |> optional "idle_doc_timeout_closed" (Decode.maybe Decode.int) Nothing
        |> optional "idle_doc_timeout_canceled" (Decode.maybe Decode.int) Nothing
        |> optional "idle_doc_timeout_timedout" (Decode.maybe Decode.int) Nothing
        |> optional "idle_doc_timeout_rejected" (Decode.maybe Decode.int) Nothing
        |> optional "idle_doc_timeout_error" (Decode.maybe Decode.int) Nothing
        |> required "immediate_trash" bool
