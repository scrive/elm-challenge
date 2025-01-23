module Scrive.Settings exposing (..)


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI
import Scrive.RetentionPolicy exposing (PolicyRec)
import Scrive.RetentionPolicy as RetentionPolicy


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
