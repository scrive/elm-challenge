module Scrive.Data.Tag exposing
    ( ArchivedTag
    , SomeTag
    , Tag
    , decoder
    , encode
    , lastValueOfArchived
    , make
    , nameOf
    , nameOfArchived
    , setValue
    , toArchived
    , valueOf
    )

import Either exposing (Either(..))
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type Tag
    = Tag
        { name : String
        , value : String
        }


type ArchivedTag
    = ArchivedTag
        { name : String
        , lastValue : Maybe String
        }


type alias SomeTag =
    Either ArchivedTag Tag


make : String -> String -> Tag
make name value =
    Tag { name = name, value = value }


archiveAs : String -> ArchivedTag
archiveAs which =
    ArchivedTag { name = which, lastValue = Nothing }


setValue : String -> Tag -> Tag
setValue newValue theTag =
    Tag { name = nameOf theTag, value = newValue }


nameOf : Tag -> String
nameOf (Tag { name }) =
    name


valueOf : Tag -> String
valueOf (Tag { value }) =
    value


nameOfArchived : ArchivedTag -> String
nameOfArchived (ArchivedTag { name }) =
    name


lastValueOfArchived : ArchivedTag -> Maybe String
lastValueOfArchived (ArchivedTag { lastValue }) =
    lastValue


toArchived : Tag -> ArchivedTag
toArchived (Tag { name, value }) =
    ArchivedTag { name = name, lastValue = Just value }



-- type alias SchrodingerTag = Either TagToRemove Tag


decoder : Decoder SomeTag
decoder =
    D.map2
        (\name mbValue ->
            case mbValue of
                Just value ->
                    Right <| make name value

                Nothing ->
                    Left <| archiveAs name
        )
        (D.field "name" D.string)
        (D.maybe <| D.field "value" D.string)


encode : SomeTag -> E.Value
encode eTag =
    case eTag of
        Right (Tag { name, value }) ->
            E.object [ ( "name", E.string name ), ( "value", E.string value ) ]

        Left (ArchivedTag { name }) ->
            E.object [ ( "name", E.string name ) ]
