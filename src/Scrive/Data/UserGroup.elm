module Scrive.Data.UserGroup exposing (..)

import Either exposing (Either)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E
import Scrive.Data.ContactDetails as ContactDetails exposing (ContactDetails)
import Scrive.Data.NullableInt as NI exposing (NullableInt)
import Scrive.Data.Settings as Settings exposing (Settings)
import Scrive.Data.Tag as Tag exposing (ArchivedTag, SomeTag, Tag)


type alias UserGroupRef =
    { id : NullableInt, name : String }


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
