module UserGroup exposing (UserGroup, decoder)

import ContactDetails exposing (ContactDetails)
import Json.Decode exposing (Decoder)
import Settings exposing (Settings)


type alias UserGroup =
    { id : String
    , parent_id : Maybe String
    , name : String
    , children : List Child
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List Tag
    }


decoder : Decoder UserGroup
decoder =
    Json.Decode.map7 UserGroup
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "parent_id" (Json.Decode.maybe Json.Decode.string))
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "children" (Json.Decode.list childDecoder))
        (Json.Decode.field "settings" Settings.decoder)
        (Json.Decode.field "contact_details" ContactDetails.decoder)
        (Json.Decode.field "tags" (Json.Decode.list tagDecoder))


type alias Child =
    { id : String
    , name : String
    }


childDecoder : Decoder Child
childDecoder =
    Json.Decode.map2 Child
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)


type Tag
    = NameValue String String
    | Name String


tagDecoder : Decoder Tag
tagDecoder =
    Json.Decode.oneOf
        [ Json.Decode.map2 NameValue
            (Json.Decode.field "name" Json.Decode.string)
            (Json.Decode.field "value" Json.Decode.string)
        , Json.Decode.map Name
            (Json.Decode.field "name" Json.Decode.string)
        ]
