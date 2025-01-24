module Scrive.NullableInt exposing (NullableInt, encode, decode)


import Json.Decode as D
import Json.Encode as E


-- The type to handle integers stored as strings in the API
-- so that it is `Nothing` when it is an improper string (should never happen)
-- or when it is `null` (could happen), and a value when string parses to integer.
-- Ensures to encode the value into string when it is defined.
-- Constructor is closed so impossible to create not from JSON.
type NullableInt = NullableInt (Maybe Int)


decode : D.Decoder NullableInt
decode =
    D.map
        (NullableInt
            << Maybe.andThen identity
            << Maybe.map String.toInt
        )
        <| D.nullable D.string


encode : NullableInt -> E.Value
encode (NullableInt mbInt) =
    Maybe.withDefault E.null
        <| Maybe.map (E.string << String.fromInt) mbInt