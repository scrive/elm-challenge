module Settings exposing (Settings, decoder)

import DataRetentionPolicy exposing (DataRetentionPolicy)
import Json.Decode exposing (Decoder)


type alias Settings =
    { inheritedFrom : Maybe String
    , dataRetentionPolicy : DataRetentionPolicy
    }


decoder : Decoder Settings
decoder =
    Json.Decode.map2 Settings
        (Json.Decode.field "inherited_from" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "data_retention_policy" DataRetentionPolicy.decoder)
