module Types.UserGroup exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode



-- we can enforce data structure (validations) at the api interface level, which would reject incorrectly formed data at the application boundary
-- or enforce structure at the form validation level and handle add the transformation step between DTOs and type-safe data structures
-- here the choice was made to match the DTO structure to JSON


type UserGroupId
    = UserGroupId Int


type alias UserGroupDto =
    { id : UserGroupId
    , parentId : Maybe UserGroupId
    , name : String
    , children : List ChildUserGroupDto
    , settings : SettingsDto
    , contactDetails : ContactDetailsDto
    , tags : List TagDto
    }


withSettings : SettingsDto -> UserGroupDto -> UserGroupDto
withSettings settings userGroup =
    { userGroup | settings = settings }


type alias ChildUserGroupDto =
    { id : UserGroupId
    , name : String
    }


type alias DataRetentionPolicyDto =
    { preparation : Maybe Int
    , closed : Maybe Int
    , canceled : Maybe Int
    , timedOut : Maybe Int
    , rejected : Maybe Int
    , error : Maybe Int
    , immediateTrash : Bool
    }


type alias SettingsDto =
    { inheritedFrom : Maybe UserGroupId
    , dataRetentionPolicy : DataRetentionPolicyDto
    }


type PreferredContactMethod
    = Email
    | Phone
    | Post


type alias AddressDto =
    { preferredContactMethod : PreferredContactMethod
    , email : Maybe String
    , phone : Maybe String
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe String
    , city : Maybe String
    , country : Maybe String
    }


type alias ContactDetailsDto =
    { inheritedFrom : Maybe UserGroupId
    , address : AddressDto
    }


type alias TagDto =
    { name : String
    , value : Maybe String
    }


decodeUserGroupId : Decode.Decoder UserGroupId
decodeUserGroupId =
    Decode.andThen parseInt Decode.string
        |> Decode.map UserGroupId


encodeUserGroupId : UserGroupId -> Encode.Value
encodeUserGroupId (UserGroupId id) =
    Encode.string <| String.fromInt id


decodeChildUserGroup : Decode.Decoder ChildUserGroupDto
decodeChildUserGroup =
    Decode.map2 ChildUserGroupDto
        (Decode.field "id" decodeUserGroupId)
        (Decode.field "name" Decode.string)


encodeChildUserGroup : ChildUserGroupDto -> Encode.Value
encodeChildUserGroup childUserGroup =
    Encode.object
        [ ( "id", encodeUserGroupId childUserGroup.id )
        , ( "name", Encode.string childUserGroup.name )
        ]


decodeDataRetentionPolicy : Decode.Decoder DataRetentionPolicyDto
decodeDataRetentionPolicy =
    Decode.map7 DataRetentionPolicyDto
        (Decode.field "idle_doc_timeout_preparation" (Decode.maybe Decode.int))
        (Decode.field "idle_doc_timeout_closed" (Decode.maybe Decode.int))
        (Decode.field "idle_doc_timeout_canceled" (Decode.maybe Decode.int))
        (Decode.field "idle_doc_timeout_timedout" (Decode.maybe Decode.int))
        (Decode.field "idle_doc_timeout_rejected" (Decode.maybe Decode.int))
        (Decode.field "idle_doc_timeout_error" (Decode.maybe Decode.int))
        (Decode.field "immediate_trash" Decode.bool)


encodeDataRetentionPolicy : DataRetentionPolicyDto -> Encode.Value
encodeDataRetentionPolicy dataRetentionPolicy =
    Encode.object
        [ ( "idle_doc_timeout_preparation", dataRetentionPolicy.preparation |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "idle_doc_timeout_closed", dataRetentionPolicy.closed |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "idle_doc_timeout_canceled", dataRetentionPolicy.canceled |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "idle_doc_timeout_timedout", dataRetentionPolicy.timedOut |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "idle_doc_timeout_rejected", dataRetentionPolicy.rejected |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "idle_doc_timeout_error", dataRetentionPolicy.error |> Maybe.map Encode.int |> Maybe.withDefault Encode.null )
        , ( "immediate_trash", Encode.bool dataRetentionPolicy.immediateTrash )
        ]


decodeSettings : Decode.Decoder SettingsDto
decodeSettings =
    Decode.map2 SettingsDto
        (Decode.field "inherited_from" (Decode.maybe decodeUserGroupId))
        (Decode.field "data_retention_policy" decodeDataRetentionPolicy)


encodeSettings : SettingsDto -> Encode.Value
encodeSettings settings =
    Encode.object
        [ ( "inherited_from", settings.inheritedFrom |> Maybe.map encodeUserGroupId |> Maybe.withDefault Encode.null )
        , ( "data_retention_policy", encodeDataRetentionPolicy settings.dataRetentionPolicy )
        ]


decodePreferredContactMethod : Decode.Decoder PreferredContactMethod
decodePreferredContactMethod =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "email" ->
                        Decode.succeed Email

                    "phone" ->
                        Decode.succeed Phone

                    "post" ->
                        Decode.succeed Post

                    _ ->
                        Decode.fail <| "Cannot parse \"" ++ str ++ "\" as a PreferredContactMethod"
            )


encodePreferredContactMethod : PreferredContactMethod -> Encode.Value
encodePreferredContactMethod preferredContactMethod =
    case preferredContactMethod of
        Email ->
            Encode.string "email"

        Phone ->
            Encode.string "phone"

        Post ->
            Encode.string "post"


decodeAddress : Decode.Decoder AddressDto
decodeAddress =
    Decode.map8 AddressDto
        (Decode.field "preferred_contact_method" decodePreferredContactMethod)
        (Decode.field "email" (Decode.maybe Decode.string))
        (Decode.field "phone" (Decode.maybe Decode.string))
        (Decode.field "company_name" (Decode.maybe Decode.string))
        (Decode.field "address" (Decode.maybe Decode.string))
        (Decode.field "zip" (Decode.maybe Decode.string))
        (Decode.field "city" (Decode.maybe Decode.string))
        (Decode.field "country" (Decode.maybe Decode.string))


encodeAddress : AddressDto -> Encode.Value
encodeAddress address =
    Encode.object
        [ ( "preferred_contact_method", encodePreferredContactMethod address.preferredContactMethod )
        , ( "email", address.email |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "phone", address.phone |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "company_name", address.companyName |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "address", address.address |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "zip", address.zip |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "city", address.city |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        , ( "country", address.country |> Maybe.map Encode.string |> Maybe.withDefault Encode.null )
        ]


decodeContactDetails : Decode.Decoder ContactDetailsDto
decodeContactDetails =
    Decode.map2 ContactDetailsDto
        (Decode.field "inherited_from" (Decode.maybe decodeUserGroupId))
        (Decode.field "address" decodeAddress)


encodeContactDetails : ContactDetailsDto -> Encode.Value
encodeContactDetails contactDetails =
    Encode.object
        [ ( "inherited_from", contactDetails.inheritedFrom |> Maybe.map encodeUserGroupId |> Maybe.withDefault Encode.null )
        , ( "address", encodeAddress contactDetails.address )
        ]


decodeTag : Decode.Decoder TagDto
decodeTag =
    Decode.map2 TagDto
        (Decode.field "name" Decode.string)
        (Decode.oneOf
            [ Decode.field "value" (Decode.maybe Decode.string)
            , Decode.succeed Nothing
            ]
        )


encodeTag : TagDto -> Encode.Value
encodeTag tag =
    case tag.value of
        Just value ->
            Encode.object
                [ ( "name", Encode.string tag.name )
                , ( "value", Encode.string value )
                ]

        Nothing ->
            Encode.object
                [ ( "name", Encode.string tag.name )
                ]


decodeUserGroupDto : Decode.Decoder UserGroupDto
decodeUserGroupDto =
    Decode.map7 UserGroupDto
        (Decode.field "id" decodeUserGroupId)
        (Decode.field "parent_id" (Decode.maybe decodeUserGroupId))
        (Decode.field "name" Decode.string)
        (Decode.field "children" (Decode.list decodeChildUserGroup))
        (Decode.field "settings" decodeSettings)
        (Decode.field "contact_details" decodeContactDetails)
        (Decode.field "tags" (Decode.list decodeTag))


encodeUserGroupDto : UserGroupDto -> Encode.Value
encodeUserGroupDto userGroup =
    Encode.object
        [ ( "id", encodeUserGroupId userGroup.id )
        , ( "parent_id", userGroup.parentId |> Maybe.map encodeUserGroupId |> Maybe.withDefault Encode.null )
        , ( "name", Encode.string userGroup.name )
        , ( "children", Encode.list encodeChildUserGroup userGroup.children )
        , ( "settings", encodeSettings userGroup.settings )
        , ( "contact_details", encodeContactDetails userGroup.contactDetails )
        , ( "tags", Encode.list encodeTag userGroup.tags )
        ]


parseInt : String -> Decode.Decoder Int
parseInt str =
    case String.toInt str of
        Just val ->
            Decode.succeed val

        Nothing ->
            Decode.fail <| "Cannot parse \"" ++ str ++ "\" as an Int."
