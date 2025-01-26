module Data.UserGroup exposing
    ( Model
    , decode
    , getContactDetails
    , getSettings
    , getTags
    , updateContactDetails
    , updateSettings
    , updateTags
    )

import Data.ContactDetails as ContactDetails
import Data.Settings as Settings
import Data.Tag as Tag
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline


type Model
    = Model UserGroup


type alias UserGroup =
    { id : String
    , parentId : String
    , name : String
    , children : List Children
    , settings : Settings.Model
    , contactDetails : ContactDetails.Model
    , tags : Tag.Tags
    }


type alias Children =
    { id : String
    , name : String
    }


decode : Decoder Model
decode =
    Decode.map Model decodeUserGroup


decodeUserGroup : Decoder UserGroup
decodeUserGroup =
    Decode.succeed UserGroup
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "parent_id" Decode.string
        |> Pipeline.required "name" Decode.string
        |> Pipeline.required "children" (Decode.list decodeChildren)
        |> Pipeline.required "settings" Settings.decode
        |> Pipeline.required "contact_details" ContactDetails.decode
        |> Pipeline.required "tags" (Decode.list Tag.decode)


decodeChildren : Decoder Children
decodeChildren =
    Decode.succeed Children
        |> Pipeline.required "id" Decode.string
        |> Pipeline.required "name" Decode.string


getSettings : Model -> Settings.Model
getSettings (Model { settings }) =
    settings


getContactDetails : Model -> ContactDetails.Model
getContactDetails (Model { contactDetails }) =
    contactDetails


getTags : Model -> Tag.Tags
getTags (Model { tags }) =
    tags


updateSettings : Settings.Model -> Model -> Model
updateSettings settings (Model userGroup) =
    Model { userGroup | settings = settings }


updateContactDetails : ContactDetails.Model -> Model -> Model
updateContactDetails contactDetails (Model userGroup) =
    Model { userGroup | contactDetails = contactDetails }


updateTags : Tag.Tags -> Model -> Model
updateTags tags (Model userGroup) =
    Model { userGroup | tags = tags }
