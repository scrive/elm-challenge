module Data.UserGroup exposing (..)

import Data exposing (userGroup)
import Json.Decode as Decode
import Json.Decode.Extra as Decode exposing (andMap)


type alias UserGroup =
    { id : String
    , parentId : Maybe String
    , name : String
    , children : List UserGroupIdentifier
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List Tag
    }


updateDataRetentionPolicy : DataRetentionPolicy -> UserGroup -> UserGroup
updateDataRetentionPolicy dataRetentionPolicy userGroup =
    let
        settings =
            userGroup.settings
    in
    { userGroup | settings = { settings | dataRetentionPolicy = dataRetentionPolicy } }


updateContactDetailsAddress : Address -> UserGroup -> UserGroup
updateContactDetailsAddress address userGroup =
    let
        contactDetails =
            userGroup.contactDetails
    in
    { userGroup | contactDetails = { contactDetails | address = address } }


decodeUserGroup : Decode.Decoder UserGroup
decodeUserGroup =
    Decode.succeed UserGroup
        |> andMap (Decode.field "id" Decode.string)
        |> andMap (Decode.field "parent_id" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "name" Decode.string)
        |> andMap (Decode.field "children" <| Decode.list decodeUserGroupIdentifier)
        |> andMap (Decode.field "settings" decodeSettings)
        |> andMap (Decode.field "contact_details" decodeContactDetails)
        |> andMap (Decode.field "tags" <| Decode.list decodeTag)


type alias UserGroupIdentifier =
    { id : String
    , name : String
    }


decodeUserGroupIdentifier : Decode.Decoder UserGroupIdentifier
decodeUserGroupIdentifier =
    Decode.succeed UserGroupIdentifier
        |> andMap (Decode.field "id" Decode.string)
        |> andMap (Decode.field "name" Decode.string)



-- Data retention policy settings


type alias Settings =
    { inheritedFrom : Maybe String
    , dataRetentionPolicy : DataRetentionPolicy
    }


decodeSettings : Decode.Decoder Settings
decodeSettings =
    Decode.succeed Settings
        |> andMap (Decode.field "inherited_from" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "data_retention_policy" decodeDataRetentionPolicy)


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


decodeDataRetentionPolicy : Decode.Decoder DataRetentionPolicy
decodeDataRetentionPolicy =
    Decode.succeed DataRetentionPolicy
        |> andMap (Decode.field "idle_doc_timeout_preparation" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "idle_doc_timeout_closed" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "idle_doc_timeout_canceled" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "idle_doc_timeout_timedout" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "idle_doc_timeout_rejected" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "idle_doc_timeout_error" <| Decode.nullable Decode.int)
        |> andMap (Decode.field "immediate_trash" Decode.bool)



-- Contact details


type alias ContactDetails =
    { inheritedFrom : Maybe String
    , address : Address
    }


decodeContactDetails : Decode.Decoder ContactDetails
decodeContactDetails =
    Decode.succeed ContactDetails
        |> andMap (Decode.field "inherited_from" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "address" decodeAddress)


type alias Address =
    { preferredContactMethod : PreferredContactMethod
    , email : Maybe String
    , phone : Maybe String
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe String
    , city : Maybe String
    , country : Maybe String
    }


decodeAddress : Decode.Decoder Address
decodeAddress =
    Decode.succeed Address
        |> andMap (Decode.field "preferred_contact_method" decodePreferredContactMethod)
        |> andMap (Decode.field "email" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "phone" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "company_name" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "address" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "zip" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "city" <| Decode.nullable Decode.string)
        |> andMap (Decode.field "country" <| Decode.nullable Decode.string)


type PreferredContactMethod
    = Email
    | Phone
    | Post


decodePreferredContactMethod : Decode.Decoder PreferredContactMethod
decodePreferredContactMethod =
    Decode.string
        |> Decode.andThen
            (\v ->
                case v of
                    "email" ->
                        Decode.succeed Email

                    "phone" ->
                        Decode.succeed Phone

                    "post" ->
                        Decode.succeed Post

                    _ ->
                        Decode.fail "Invalid preferred contact method"
            )



-- Tags


type alias Tag =
    { name : String
    , value : Maybe String
    }


decodeTag : Decode.Decoder Tag
decodeTag =
    Decode.succeed Tag
        |> andMap (Decode.field "name" Decode.string)
        |> andMap (Decode.maybe <| Decode.field "value" Decode.string)



-- Retrieval


fetchUserGroup : Result Decode.Error UserGroup
fetchUserGroup =
    Decode.decodeString decodeUserGroup userGroup
