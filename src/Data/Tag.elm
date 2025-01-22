module Data.Tag exposing
    ( Tag, TagToRemove
    , make, toRemove
    , setValue, toRemoved
    , nameOf, valueOf, nameOfRemoved
    , decoder, encode
    )


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


make : String -> String -> Tag
make name value = Tag { name = name, value = value }


toRemove : String -> TagToRemove
toRemove which = TagToRemove { name = which }


setValue : String -> Tag -> Tag
setValue newValue theTag = Tag { name = nameOf theTag, value = newValue }


nameOf : Tag -> String
nameOf (Tag { name }) = name


valueOf : Tag -> String
valueOf (Tag { value }) = value


nameOfRemoved : TagToRemove -> String
nameOfRemoved (TagToRemove { name }) = name


toRemoved : Tag -> TagToRemove
toRemoved (Tag { name }) = TagToRemove { name = name }


-- type alias SchrodingerTag = Either TagToRemove Tag


decoder : Decoder (Either TagToRemove Tag)
decoder =
    D.map2
        (\name mbValue ->
            case mbValue of
                Just value -> Right <| make name value
                Nothing    -> Left  <| toRemove name
        )
        (D.field "name" D.string)
        (D.maybe <| D.field "value" D.string)


encode : Either TagToRemove Tag -> E.Value
encode eTag =
    case eTag of
        Right ( Tag { name, value} )   -> E.object [ ( "name", E.string name ), ( "value", E.string value ) ]
        Left  ( TagToRemove { name } ) -> E.object [ ( "name", E.string name ) ]