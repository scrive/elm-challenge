module Data exposing
  ( data
  , decodeUserGroup
  , UserGroup, UserGroupChild, Settings, ContactDetails, Address, Tag
  )


import Json.Decode as D


type alias UserGroup =
  { id : String
  , parentId : String
  , name : String
  , children : List UserGroupChild
  , settings : Settings
  , contactDetails : ContactDetails
  , tags : List Tag
  }


type alias UserGroupChild =
  { id : String
  , name : String
  }


type alias Settings =
  { inheritedFrom : Maybe String
  , dataRetentionPolicy : DataRetentionPolicy
  }


type alias DataRetentionPolicy =
  { preparation : Maybe Int
  , closed : Maybe Int
  , canceled : Maybe Int
  , timeout : Maybe Int
  , rejected : Maybe Int
  , error : Maybe Int
  , immediateTrash : Bool
  }


type alias ContactDetails =
  { inheritedFrom : Maybe String
  , address : Address
  }


type alias Address =
  { preferredContactMethod : ContactMethod
  , email : Maybe String
  , phone : Maybe String
  , companyName : String
  , address : String
  , zip : String
  , city : String
  , country : String
  }


type ContactMethod
  = Email
  | Phone
  | Post


type alias Tag =
  { name : String
  , value : Maybe String
  }


decodeUserGroup : D.Decoder UserGroup
decodeUserGroup =
  D.map7 UserGroup
    (D.field "id" D.string)
    (D.field "parent_id" D.string)
    (D.field "name" D.string)
    (D.field "children" (D.list decodeUserGroupChild))
    (D.field "settings" decodeSettings)
    (D.field "contact_details" decodeContactDetails)
    (D.field "tags" (D.list decodeTag))


decodeUserGroupChild : D.Decoder UserGroupChild
decodeUserGroupChild =
  D.map2 UserGroupChild
    (D.field "id" D.string)
    (D.field "name" D.string)


decodeSettings : D.Decoder Settings
decodeSettings =
  D.map2 Settings
    (D.field "inherited_from" (D.nullable D.string))
    (D.field "data_retention_policy" decodeDataRetentionPolicy)


decodeDataRetentionPolicy : D.Decoder DataRetentionPolicy
decodeDataRetentionPolicy =
  D.map7 DataRetentionPolicy
    (D.field "idle_doc_timeout_preparation" (D.nullable D.int))
    (D.field "idle_doc_timeout_closed" (D.nullable D.int))
    (D.field "idle_doc_timeout_canceled" (D.nullable D.int))
    (D.field "idle_doc_timeout_timedout" (D.nullable D.int))
    (D.field "idle_doc_timeout_rejected" (D.nullable D.int))
    (D.field "idle_doc_timeout_error" (D.nullable D.int))
    (D.field "immediate_trash" D.bool)


decodeContactDetails : D.Decoder ContactDetails
decodeContactDetails =
  D.map2 ContactDetails
    (D.field "inherited_from" (D.nullable D.string))
    (D.field "address" decodeAddress)


decodeAddress : D.Decoder Address
decodeAddress =
  D.map8 Address
    (D.field "preferred_contact_method" decodeContactMethod)
    (D.field "email" (D.nullable D.string))
    (D.field "phone" (D.nullable D.string))
    (D.field "company_name" D.string)
    (D.field "address" D.string)
    (D.field "zip" D.string)
    (D.field "city" D.string)
    (D.field "country" D.string)


decodeContactMethod : D.Decoder ContactMethod
decodeContactMethod =
  let toContactMethod str =
        case str of
          "email" -> D.succeed Email
          "phone" -> D.succeed Phone
          "post" -> D.succeed Post
          _ -> D.fail "Invalid contact method"
  in
  D.andThen toContactMethod D.string


decodeTag : D.Decoder Tag
decodeTag =
  D.map2 Tag
    (D.field "name" D.string)
    (D.oneOf [ D.map Just (D.field "value" D.string), D.succeed Nothing ])


data : String
data =
    """{
  "id": "5",
  "parent_id": "1",
  "name": "A Child Usergroup",
  "children": [
    {
      "id": "2",
      "name": "Some child user group"
    },
    {
      "id": "3",
      "name": "Yet another child user group"
    }
  ],
  "settings": {
    "inherited_from": null,
    "data_retention_policy": {
      "idle_doc_timeout_preparation": null,
      "idle_doc_timeout_closed": null,
      "idle_doc_timeout_canceled": 42,
      "idle_doc_timeout_timedout": null,
      "idle_doc_timeout_rejected": null,
      "idle_doc_timeout_error": null,
      "immediate_trash": false
    }
  },
  "contact_details": {
    "inherited_from": "1",
    "address": {
      "preferred_contact_method": "email",
      "email": "scrive@scrive.com",
      "phone": null,
      "company_name": "Scrive",
      "address": "Grev Turegatan 11A",
      "zip": "114 46",
      "city": "Stockholm",
      "country": "Sweden"
    }
  },
  "tags": [
    {
      "name": "founded",
      "value": "1846"
    },
    {
      "name": "status",
      "value": "busy"
    },
    {
      "name": "2FA enabled"
    }
  ]
}"""
