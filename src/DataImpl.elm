module DataImpl exposing (..)

import Either exposing (Either)

import Data.Settings exposing (Settings)
import Data.Settings as Settings
import Data.ContactDetails exposing (ContactDetails)
import Data.ContactDetails as ContactDetails
import Data.Tag exposing (Tag, TagToRemove)
import Data.Tag as Tag

import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type alias Data =
    { id : Int
    , parentId : Int
    , name : String
    -- , children : List UserGroup -- TODO
    , settings : Settings
    , contactDetails : ContactDetails
    , tags : List (Either TagToRemove Tag)
    }


decoder : Decoder Data
decoder =
    D.map6
        Data
        (D.field "id" D.int)
        (D.field "parentId" D.int)
        (D.field "name" D.string)
        -- TODO: children
        (D.field "settings" Settings.decoder)
        (D.field "contactDetails" ContactDetails.decoder)
        (D.field "tags" <| D.list <| Tag.decoder)


encode : Data -> E.Value
encode rec =
    E.object
        [ ( "id", E.int rec.id )
        , ( "parent_id", E.int rec.parentId )
        , ( "name", E.string rec.name )
        -- TODO: children
        , ( "settings", Settings.encode rec.settings )
        , ( "contact_details", ContactDetails.encode rec.contactDetails )
        , ( "tags", E.list Tag.encode rec.tags )
        ]
