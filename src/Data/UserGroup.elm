module Data.UserGroup exposing
    ( Address
    , ContactDetails
    , ContactMethod(..)
    , Inheritable(..)
    , Settings
    , UserGroup
    , UserGroupChild
    , addressOfUserGroup
    , contactMethodFromString
    , contactMethodToString
    , decode
    , decodeAddress
    , uuidToString
    )

import Dict exposing (Dict)
import Json.Decode as JD
import Json.Decode.Pipeline exposing (optional, required)
import Maybe.Extra
import Monocle.Lens as Lens exposing (Lens)


type alias UserGroup =
    { id : Uuid
    , parentId : Maybe Uuid
    , name : String
    , children : List UserGroupChild
    , settings : Inheritable Settings
    , contactDetails : Inheritable ContactDetails
    , tags : Tags
    }


type Uuid
    = Uuid String


type alias UserGroupChild =
    { id : Uuid
    , name : String
    }


type Inheritable a
    = Inherited Uuid a
    | Uninherited a


type alias Settings =
    { dataRetentionPolicy : DataRetentionPolicy }


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedOut : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


type alias ContactDetails =
    { address : Address }


type alias Address =
    { email : String
    , phone : String
    , preferredContactMethod : ContactMethod
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


type ContactMethod
    = Post
    | Phone
    | Email


type alias Tags =
    Dict String String


decode : JD.Decoder UserGroup
decode =
    JD.succeed UserGroup
        |> required "id" (JD.string |> JD.map Uuid)
        |> optional "parent_id" (JD.string |> JD.map (Uuid >> Just)) Nothing
        |> required "name" JD.string
        |> required "children" (JD.list decodeUserGroupChild)
        |> required "settings" (decodeInheritable decodeSettings)
        |> required "contact_details" (decodeInheritable decodeContactDetails)
        |> required "tags" decodeTags


decodeUserGroupChild : JD.Decoder UserGroupChild
decodeUserGroupChild =
    JD.succeed UserGroupChild
        |> required "id" (JD.string |> JD.map Uuid)
        |> required "name" JD.string


decodeInheritable : JD.Decoder a -> JD.Decoder (Inheritable a)
decodeInheritable decodeValue =
    JD.oneOf
        [ JD.field "inherited_from" (JD.string |> JD.map Uuid)
            |> JD.andThen (decodeInherited decodeValue)
        , decodeValue |> JD.map Uninherited
        ]


decodeInherited : JD.Decoder a -> Uuid -> JD.Decoder (Inheritable a)
decodeInherited decodeValue inheritedFrom =
    JD.map (Inherited inheritedFrom) decodeValue


decodeSettings : JD.Decoder Settings
decodeSettings =
    JD.succeed Settings
        |> required "data_retention_policy" decodDataRetentionPolicy


decodDataRetentionPolicy : JD.Decoder DataRetentionPolicy
decodDataRetentionPolicy =
    JD.succeed DataRetentionPolicy
        |> optional "idle_doc_timeout_preparation" (JD.int |> JD.map Just) Nothing
        |> optional "idle_doc_timeout_closed" (JD.int |> JD.map Just) Nothing
        |> optional "idle_doc_timeout_canceled" (JD.int |> JD.map Just) Nothing
        |> optional "idle_doc_timeout_timedout" (JD.int |> JD.map Just) Nothing
        |> optional "idle_doc_timeout_rejected" (JD.int |> JD.map Just) Nothing
        |> optional "idle_doc_timeout_error" (JD.int |> JD.map Just) Nothing
        |> required "immediate_trash" JD.bool


decodeContactDetails : JD.Decoder ContactDetails
decodeContactDetails =
    JD.succeed ContactDetails
        |> required "address" decodeAddress


decodeAddress : JD.Decoder Address
decodeAddress =
    JD.succeed Address
        |> optional "email" JD.string ""
        |> optional "phone" JD.string ""
        |> required "preferred_contact_method" decodeContactMethod
        |> required "company_name" JD.string
        |> required "address" JD.string
        |> required "zip" JD.string
        |> required "city" JD.string
        |> required "country" JD.string


decodeContactMethod : JD.Decoder ContactMethod
decodeContactMethod =
    JD.string
        |> JD.map contactMethodFromString
        |> JD.andThen
            (Maybe.Extra.unwrap (JD.fail "Unknown contact method") JD.succeed)


contactMethodFromString : String -> Maybe ContactMethod
contactMethodFromString str =
    case str of
        "post" ->
            Just Post

        "phone" ->
            Just Phone

        "email" ->
            Just Email

        _ ->
            Nothing


contactMethodToString : ContactMethod -> String
contactMethodToString str =
    case str of
        Post ->
            "post"

        Phone ->
            "phone"

        Email ->
            "email"


decodeTags : JD.Decoder Tags
decodeTags =
    JD.succeed Tuple.pair
        |> required "name" JD.string
        |> optional "value" JD.string ""
        |> JD.list
        |> JD.map Dict.fromList


contactDetailsOfUserGroup : Lens UserGroup ContactDetails
contactDetailsOfUserGroup =
    Lens
        (\userGroup ->
            case userGroup.contactDetails of
                Inherited _ c ->
                    c

                Uninherited c ->
                    c
        )
        (\b a ->
            { a
                | contactDetails =
                    case a.contactDetails of
                        Inherited u _ ->
                            Inherited u b

                        Uninherited _ ->
                            Uninherited b
            }
        )


addressOfContactDetails : Lens ContactDetails Address
addressOfContactDetails =
    Lens .address (\b a -> { a | address = b })


addressOfUserGroup : Lens UserGroup Address
addressOfUserGroup =
    Lens.compose contactDetailsOfUserGroup addressOfContactDetails


uuidToString : Uuid -> String
uuidToString (Uuid str) =
    str
