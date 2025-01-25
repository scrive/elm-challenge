module Scrive.Data.ContactDetails exposing (..)

import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Scrive.Data.Address as Address exposing (Address)
import Scrive.Data.NullableInt as NI exposing (NullableInt)


type alias ContactDetails =
    { inheritedFrom : NullableInt
    , address : Address
    }


decoder : Decoder ContactDetails
decoder =
    D.map2
        ContactDetails
        (D.field "inherited_from" <| NI.decode)
        (D.field "address" Address.decoder)


encode : ContactDetails -> E.Value
encode rec =
    E.object
        [ ( "inherited_from", NI.encode rec.inheritedFrom )
        , ( "address", Address.encode rec.address )
        ]
