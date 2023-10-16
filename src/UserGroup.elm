module UserGroup exposing (UserGroup, Settings, DataRetentionPolicy, ContactDetails, Address, Tags, decoder)

import Dict exposing (Dict)
import Json.Decode exposing (..)


type alias UserGroup =
    { id : String
    , parentId : String
    , name : String
    , children : List Child
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : Tags
    }


type alias Child =
    { id : String
    , name : String
    }


type alias Settings =
    { inheritedFrom : Maybe String
    , dataRetentionPolicy : DataRetentionPolicy
    }


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedout : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


type alias ContactDetails =
    { inheritedFrom : Maybe String
    , address : Address
    }


type alias Address =
    { preferredContactMethod : String
    , email : Maybe String
    , phone : Maybe String
    , companyName : Maybe String
    , address : Maybe String
    , zip : Maybe String
    , city : Maybe String
    , country : Maybe String
    }


type alias Tags =
    Dict String (Maybe String)


decoder : Decoder UserGroup
decoder =
    map7 UserGroup
        (field "id" string)
        (field "parent_id" string)
        (field "name" string)
        (field "children" (list child))
        (field "settings" settings)
        (field "contact_details" contactDetails)
        (field "tags" tags)


child : Decoder Child
child =
    map2 Child
        (field "id" string)
        (field "name" string)


settings : Decoder Settings
settings =
    map2 Settings
        (field "inherited_from" (nullable string))
        (field "data_retention_policy" dataRetentionPolicy)


dataRetentionPolicy : Decoder DataRetentionPolicy
dataRetentionPolicy =
    map7 DataRetentionPolicy
        (field "idle_doc_timeout_preparation" (nullable int))
        (field "idle_doc_timeout_closed" (nullable int))
        (field "idle_doc_timeout_canceled" (nullable int))
        (field "idle_doc_timeout_timedout" (nullable int))
        (field "idle_doc_timeout_rejected" (nullable int))
        (field "idle_doc_timeout_error" (nullable int))
        (field "immediate_trash" bool)


contactDetails : Decoder ContactDetails
contactDetails =
    map2 ContactDetails
        (field "inherited_from" (nullable string))
        (field "address" address)


address : Decoder Address
address =
    map8 Address
        (field "preferred_contact_method" string)
        (field "email" (nullable string))
        (field "phone" (nullable string))
        (field "company_name" (nullable string))
        (field "address" (nullable string))
        (field "zip" (nullable string))
        (field "city" (nullable string))
        (field "country" (nullable string))


tags : Decoder Tags
tags =
    let
        tag =
            map2 Tuple.pair
                (field "name" string)
                (maybe (field "value" string))
    in
        list tag |> andThen (\t -> succeed (Dict.fromList t))
        
