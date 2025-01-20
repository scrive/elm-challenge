module Data.Tag exposing (Tag, TagToRemove, tag, remove, decoder, encode)


import Either exposing (Either(..))


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E


type Tag =
    Tag
        { name : String
        , value : String
        }


type TagToRemove = TagToRemove { name : String }


tag : String -> String -> Tag
tag name value = Tag { name = name, value = value }


remove : String -> TagToRemove
remove which = TagToRemove { name = which }


decoder : Decoder (Either TagToRemove Tag)
decoder =
    D.map2
        (\name mbValue ->
            case mbValue of
                Just value -> Right <| tag name value
                Nothing    -> Left  <| remove name
        )
        (D.field "name" D.string)
        (D.nullable <| D.field "value" D.string)


encode : Either TagToRemove Tag -> E.Value
encode eTag =
    case eTag of
        Right ( Tag { name, value} )   -> E.object [ ( "name", E.string name ), ( "value", E.string value ) ]
        Left  ( TagToRemove { name } ) -> E.object [ ( "name", E.string name ) ]
