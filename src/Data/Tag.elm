module Data.Tag exposing
    ( Tag, TagToRemove
    , tag, remove
    , nameOf, valueOf, nameOfRemoved
    , decoder, encode
    )


import Either exposing (Either(..))


import Json.Decode exposing (Decoder)
import Json.Decode as D
import Json.Encode as E

import Validate as V


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


-- type alias SchrodingerTag = Either TagToRemove Tag


decoder : Decoder (Either TagToRemove Tag)
decoder =
    D.map2
        (\name mbValue ->
            case mbValue of
                Just value -> Right <| tag name value
                Nothing    -> Left  <| remove name
        )
        (D.field "name" D.string)
        (D.maybe <| D.field "value" D.string)


encode : Either TagToRemove Tag -> E.Value
encode eTag =
    case eTag of
        Right ( Tag { name, value} )   -> E.object [ ( "name", E.string name ), ( "value", E.string value ) ]
        Left  ( TagToRemove { name } ) -> E.object [ ( "name", E.string name ) ]


nameOf : Tag -> String
nameOf (Tag { name }) = name


valueOf : Tag -> String
valueOf (Tag { value }) = value


nameOfRemoved : TagToRemove -> String
nameOfRemoved (TagToRemove { name }) = name


{-
tagValidator : V.Validator String Tag
tagValidator =
    V.ifTrue (nameOf >> String.length >> ((<) 32)) "Foo"
-}