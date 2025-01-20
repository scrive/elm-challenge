module Data.ContactDetails exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E

import Data.NullableInt exposing (NullableInt)
import Data.NullableInt as NI


type PreferredContact
    = PC_Email
    | PC_Post
    | PC_Phone
    | PC_None


type Email = Email String


type ZipCode = ZipCode String -- TODO: Improve later


type Phone = Phone String -- TODO: Improve later


type alias Address =
    { preferredContactMethod : PreferredContact
    , email : Email
    , phone : Maybe Phone
    , companyName : String
    , address : String
    , zip : ZipCode
    , city : String
    , country : String
    }


type alias ContactDetails =
    { inheritedFrom : NullableInt
    , address : Address
    }


addressDecoder : Decoder Address
addressDecoder =
    let
        decodeContactMethod str =
            case str of
                "email" -> PC_Email
                "post"  -> PC_Post
                "phone" -> PC_Phone
                _ -> PC_None
    in D.map8
        Address
        (D.field "preferred_contact_method" <| D.map decodeContactMethod <| D.string)
        (D.field "email" <| D.map Email <| D.string)
        (D.field "phone" <| D.map (Maybe.map Phone) <| D.nullable D.string)
        (D.field "company_name" D.string)
        (D.field "address" D.string)
        (D.field "zip" <| D.map ZipCode <| D.string)
        (D.field "city" D.string)
        (D.field "country" <| D.string)


encodeAddress : Address -> E.Value
encodeAddress rec =
    let
        encodeContactMethod m =
            case m of
                PC_Email -> E.string "email"
                PC_Post  -> E.string "post"
                PC_Phone -> E.string "phone"
                PC_None  -> E.null
    in E.object
        [ ( "preferred_contact_method", encodeContactMethod rec.preferredContactMethod )
        ]


decoder : Decoder ContactDetails
decoder =
    D.map2
        ContactDetails
        (D.field "inherited_from" <| NI.decode)
        (D.field "address" addressDecoder)


encode : ContactDetails -> E.Value
encode rec =
    E.object
        [ ( "inherited_from", NI.encode rec.inheritedFrom )
        , ( "address", encodeAddress rec.address )
        ]