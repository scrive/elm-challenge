module UserGroup exposing (Account(..), Child, UserGroup, decoder)

import ContactDetails exposing (ContactDetails)
import Json.Decode exposing (Decoder)
import Settings exposing (Settings)
import Tag exposing (Tag)


type alias UserGroup =
    { id : String
    , account : Account
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
        accountDecoder
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "children" (Json.Decode.list childDecoder))
        (Json.Decode.field "settings" Settings.decoder)
        (Json.Decode.field "contact_details" ContactDetails.decoder)
        (Json.Decode.field "tags" (Json.Decode.list Tag.decoder))


type Account
    = Root
    | Parent String


accountDecoder : Decoder Account
accountDecoder =
    Json.Decode.field "parent_id" (Json.Decode.maybe Json.Decode.string)
        |> Json.Decode.andThen
            (\maybeParentId ->
                case maybeParentId of
                    Just parentId ->
                        Json.Decode.succeed (Parent parentId)

                    Nothing ->
                        Json.Decode.succeed Root
            )


type alias Child =
    { id : String
    , name : String
    }


childDecoder : Decoder Child
childDecoder =
    Json.Decode.map2 Child
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
