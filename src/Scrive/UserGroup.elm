module Scrive.UserGroup exposing (..)

import Either exposing (Either)

import Scrive.Settings exposing (Settings)
import Scrive.Settings as Settings
import Scrive.ContactDetails exposing (ContactDetails)
import Scrive.ContactDetails as ContactDetails
import Scrive.Tag exposing (Tag, ArchivedTag, SomeTag)
import Scrive.Tag as Tag
import Scrive.NullableInt exposing (NullableInt)
import Scrive.NullableInt as NI

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type alias UserGroupRef = { id : NullableInt, name : String }


type alias UserGroup =
    { id : NullableInt
    , parentId : NullableInt
    , name : String
    , children : List UserGroupRef
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List SomeTag
    }


decoder : Decoder UserGroup
decoder =
    D.map7
        UserGroup
        (D.field "id" NI.decode)
        (D.field "parent_id" NI.decode)
        (D.field "name" D.string)
        (D.field "children" <| D.list childRefDecoder)
        (D.field "settings" Settings.decoder)
        (D.field "contact_details" ContactDetails.decoder)
        (D.field "tags" <| D.list <| Tag.decoder)


encode : UserGroup -> E.Value
encode rec =
    E.object
        [ ( "id", NI.encode rec.id )
        , ( "parent_id", NI.encode rec.parentId )
        , ( "name", E.string rec.name )
        , ( "children", E.list childRefEncoder rec.children )
        , ( "settings", Settings.encode rec.settings )
        , ( "contact_details", ContactDetails.encode rec.contactDetails )
        , ( "tags", E.list Tag.encode rec.tags )
        ]


childRefDecoder : Decoder UserGroupRef
childRefDecoder =
    D.map2
        UserGroupRef
        (D.field "id" NI.decode)
        (D.field "name" D.string)


childRefEncoder : UserGroupRef -> E.Value
childRefEncoder rec =
    E.object
        [ ( "id", NI.encode rec.id )
        , ( "name", E.string rec.name )
        ]