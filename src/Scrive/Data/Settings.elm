module Scrive.Data.Settings exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Scrive.Data.NullableInt as NI exposing (NullableInt)
import Scrive.Data.RetentionPolicy as RetentionPolicy exposing (PolicyRec)


type alias Settings =
    { inheritedFrom : NullableInt
    , policy : PolicyRec
    }


decoder : Decoder Settings
decoder =
    D.map2
        Settings
        (D.field "inherited_from" NI.decode)
        (D.field "data_retention_policy" RetentionPolicy.decoder)


encode : Settings -> E.Value
encode rec =
    E.object
        [ ( "inherited_from", NI.encode rec.inheritedFrom )
        , ( "data_retention_policy", RetentionPolicy.encode rec.policy )
        ]
