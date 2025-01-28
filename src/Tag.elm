module Tag exposing (Error, Tag, decoder, errorDescription, make, toString)

import Json.Decode exposing (Decoder)


type alias Tag =
    { name : String
    , maybeValue : Maybe String
    }


decoder : Decoder Tag
decoder =
    Json.Decode.map2 Tag
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.maybe (Json.Decode.field "value" Json.Decode.string))


make : List String -> String -> Result Error Tag
make existing string =
    if String.isEmpty string then
        Err Empty

    else
        case String.split ":" string of
            [] ->
                Err Malformed

            name :: rest ->
                if List.member name existing then
                    Err NameAlreadyTaken

                else if String.length name > 32 then
                    Err NameTooLong

                else
                    case rest of
                        [] ->
                            Ok { name = String.trim name, maybeValue = Nothing }

                        value :: [] ->
                            Ok { name = String.trim name, maybeValue = Just (String.trim value) }

                        _ ->
                            Err Malformed


type Error
    = Empty
    | NameTooLong
    | Malformed
    | NameAlreadyTaken


errorDescription : Error -> String
errorDescription error =
    case error of
        Empty ->
            "String is empty."

        NameTooLong ->
            "Tag name should be less than 32 chars long."

        Malformed ->
            "Tag is malformed."

        NameAlreadyTaken ->
            "Tag name is already taken."


toString : { a | name : String, maybeValue : Maybe String } -> String
toString { name, maybeValue } =
    case maybeValue of
        Just value ->
            name ++ ":" ++ value

        Nothing ->
            name
