module Scrive.ContactDetails exposing (..)

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E

import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI


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
    , email : Maybe Email
    , phone : Maybe Phone
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe ZipCode
    , city : Maybe String
    , country : Maybe String
    }


type alias ContactDetails =
    { inheritedFrom : NullableInt
    , address : Address
    }


emailToString : Email -> String
emailToString (Email email) = email


zipCodeToString : ZipCode -> String
zipCodeToString (ZipCode zip) = zip


phoneToString : Phone -> String
phoneToString (Phone phone) = phone


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
        (D.field "email" <| D.map (Maybe.map Email) <| D.nullable D.string)
        (D.field "phone" <| D.map (Maybe.map Phone) <| D.nullable D.string)
        (D.field "company_name" <| D.nullable D.string)
        (D.field "address" <| D.nullable D.string)
        (D.field "zip" <| D.map (Maybe.map ZipCode) <| D.nullable D.string)
        (D.field "city" <| D.nullable D.string)
        (D.field "country" <| D.nullable D.string)


encodeAddress : Address -> E.Value
encodeAddress rec =
    let
        encodeContactMethod m =
            case m of
                PC_Email -> E.string "email"
                PC_Post  -> E.string "post"
                PC_Phone -> E.string "phone"
                PC_None  -> E.null
        encodeField = Maybe.map E.string >> Maybe.withDefault E.null
    in E.object
        [ ( "preferred_contact_method", encodeContactMethod rec.preferredContactMethod )
        , ( "email", encodeField <| Maybe.map emailToString rec.email )
        , ( "phone", encodeField <| Maybe.map phoneToString rec.phone )
        , ( "company_name", encodeField rec.companyName )
        , ( "address", encodeField rec.address )
        , ( "zip", encodeField <| Maybe.map zipCodeToString rec.zip )
        , ( "city", encodeField rec.city )
        , ( "country", encodeField rec.country )
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