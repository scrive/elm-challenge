module Scrive.ContactDetails exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E

import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI
import Scrive.Address exposing (Address)
import Scrive.Address as Address


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