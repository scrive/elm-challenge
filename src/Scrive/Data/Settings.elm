module Scrive.Data.Settings exposing (..)


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


import Scrive.Data.NullableInt exposing (NullableInt)
import Scrive.Data.NullableInt as NI
import Scrive.Data.RetentionPolicy exposing (PolicyRec)
import Scrive.Data.RetentionPolicy as RetentionPolicy


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
