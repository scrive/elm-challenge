module Scrive.UserGroup exposing (..)

import Either exposing (Either)

import Scrive.Settings exposing (Settings)
import Scrive.Settings as Settings
import Scrive.ContactDetails exposing (ContactDetails)
import Scrive.ContactDetails as ContactDetails
import Scrive.Tag exposing (Tag, TagToRemove)
import Scrive.Tag as Tag
import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type alias TagList = List (Either TagToRemove Tag)


type alias UserGroup =
    { id : NullableInt
    , parentId : NullableInt
    , name : String
    -- , children : List UserGroup -- TODO
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : TagList
    }


decoder : Decoder UserGroup
decoder =
    D.map6
        UserGroup
        (D.field "id" NI.decode)
        (D.field "parent_id" NI.decode)
        (D.field "name" D.string)
        -- TODO: children
        (D.field "settings" Settings.decoder)
        (D.field "contact_details" ContactDetails.decoder)
        (D.field "tags" <| D.list <| Tag.decoder)


encode : UserGroup -> E.Value
encode rec =
    E.object
        [ ( "id", NI.encode rec.id )
        , ( "parent_id", NI.encode rec.parentId )
        , ( "name", E.string rec.name )
        -- TODO: children
        , ( "settings", Settings.encode rec.settings )
        , ( "contact_details", ContactDetails.encode rec.contactDetails )
        , ( "tags", E.list Tag.encode rec.tags )
        ]
