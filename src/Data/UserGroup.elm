module Data.UserGroup exposing (UserGroup)


type alias UserGroup =
    { id : Uuid
    , parentId : Maybe Uuid
    , name : String
    , children : List UserGroupChild
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List Tag
    }


type Uuid
    = Uuid String


type alias UserGroupChild =
    { id : Uuid
    , name : String
    }


type alias Settings =
    { inheritedFrom : Maybe Uuid
    , address : Address
    }


type alias Address =
    { email : Maybe String
    , phone : Maybe String
    , preferredContactMethod : String
    , companyName : String
    , address : String
    , zip : String
    , city : String
    , country : String
    }


type alias ContactDetails =
    { inheritedFrom : Maybe Uuid
    , dataRetentionPolicy : DataRetentionPolicy
    }


type alias DataRetentionPolicy =
    { idleDocTimeoutPreparation : Maybe Int
    , idleDocTimeoutClosed : Maybe Int
    , idleDocTimeoutCanceled : Maybe Int
    , idleDocTimeoutTimedOut : Maybe Int
    , idleDocTimeoutRejected : Maybe Int
    , idleDocTimeoutError : Maybe Int
    , immediateTrash : Bool
    }


type alias Tag =
    { name : String
    , value : Maybe String
    }
