module Scrive.Data.Address exposing
    ( PreferredContact(..)
    , Field(..)
    , Address, DraftAddress
    , encode, decoder
    , preferredWaysToContact, allFields
    , emailToString, phoneToString, zipCodeToString, preferredContactToString
    , preferredContactFromOption, preferredContactToOption
    , fieldToLabel
    , draftValueOf, setDraftValue
    , toDraft, fromDraft
    )


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E

import Validate as V


type PreferredContact
    = PC_Email
    | PC_Post
    | PC_Phone
    | PC_None


type Field
    = F_Email
    | F_Phone
    | F_CompanyName
    | F_StreetAddress
    | F_ZipCode
    | F_City
    | F_Country


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


type alias DraftAddress =
    { email : Maybe String
    , phone : Maybe String
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe String
    , city : Maybe String
    , country : Maybe String
    }


emailToString : Email -> String
emailToString (Email email) = email


zipCodeToString : ZipCode -> String
zipCodeToString (ZipCode zip) = zip


phoneToString : Phone -> String
phoneToString (Phone phone) = phone


allFields : List Field
allFields = [ F_Email, F_Phone, F_CompanyName, F_StreetAddress, F_ZipCode, F_City, F_Country ]


preferredWaysToContact : List PreferredContact
preferredWaysToContact = [ PC_Email, PC_Phone, PC_Post ]


preferredContactToString : PreferredContact -> String
preferredContactToString pc =
    case pc of
        PC_Email -> "E-mail"
        PC_Post -> "Post"
        PC_Phone -> "Phone"
        PC_None -> "[None]"


preferredContactToOption : PreferredContact -> String
preferredContactToOption pc =
    case pc of
        PC_Email -> "E-mail"
        PC_Post -> "Post"
        PC_Phone -> "Phone"
        PC_None -> "[None]"


-- for forms purpose : should be the exact reverse of `preferredContactToOption`
preferredContactFromOption : String -> PreferredContact
preferredContactFromOption str =
    case str of
        "E-mail" -> PC_Email
        "Post" -> PC_Post
        "Phone" -> PC_Phone
        _ -> PC_None


decoder : Decoder Address
decoder =
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


encode : Address -> E.Value
encode rec =
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


{-
stringValueOf : Address -> Field -> Maybe String
stringValueOf rec field =
    case field of
        F_Email -> Maybe.map emailToString rec.email
        F_Phone -> Maybe.map phoneToString rec.phone
        F_CompanyName -> rec.companyName
        F_StreetAddress -> rec.address
        F_ZipCode -> Maybe.map zipCodeToString rec.zip
        F_City -> rec.city
        F_Country -> rec.country
-}


draftValueOf : DraftAddress -> Field -> Maybe String
draftValueOf rec field =
    case field of
        F_Email -> rec.email
        F_Phone -> rec.phone
        F_CompanyName -> rec.companyName
        F_StreetAddress -> rec.address
        F_ZipCode -> rec.zip
        F_City -> rec.city
        F_Country -> rec.country


toDraft : Address -> DraftAddress
toDraft rec =
    { email = Maybe.map emailToString rec.email
    , phone = Maybe.map phoneToString rec.phone
    , companyName = rec.companyName
    , address = rec.address
    , zip = Maybe.map zipCodeToString rec.zip
    , city = rec.city
    , country = rec.country
    }


fromDraft : PreferredContact -> V.Valid DraftAddress -> Address
fromDraft pcontact validDraft =
    let
        drec = V.fromValid validDraft
    in
        { preferredContactMethod = pcontact
        , email = Maybe.map Email drec.email
        , phone = Maybe.map Phone drec.phone
        , companyName = drec.companyName
        , address = drec.address
        , zip = Maybe.map ZipCode drec.zip
        , city = drec.city
        , country = drec.country
        }



setDraftValue : DraftAddress -> Field -> String -> DraftAddress
setDraftValue drec field value =
    case field of
        F_Email -> { drec | email = Just value }
        F_Phone -> { drec | phone = Just value }
        F_ZipCode -> { drec | zip = Just value }
        F_CompanyName -> { drec | companyName = Just value }
        F_StreetAddress -> { drec | address = Just value }
        F_City -> { drec | city = Just value }
        F_Country -> { drec | country = Just value }


fieldToLabel : Field -> String
fieldToLabel f =
    case f of
        F_Email -> "E-mail"
        F_Phone -> "Phone"
        F_CompanyName -> "Company Name"
        F_StreetAddress -> "Street Address"
        F_ZipCode -> "ZIP Code"
        F_City -> "City"
        F_Country -> "Country"