module Common.Validation exposing
    ( Codec
    , CodecError
    , Positive
    , compose
    , getPositive
    , int
    , positive
    , showError
    , string
    )


type CodecError
    = CodecError String


type alias Codec i o =
    { decode : i -> Result CodecError o
    , encode : o -> i
    , name : String
    }


showError : CodecError -> String
showError (CodecError msg) =
    msg


compose : Codec b c -> Codec a b -> Codec a c
compose codecBC codecAB =
    Codec
        (codecAB.decode >> Result.andThen codecBC.decode)
        (codecBC.encode >> codecAB.encode)
        (codecAB.name ++ " >> " ++ codecBC.name)


type Positive number
    = Positive number


getPositive : Positive number -> number
getPositive (Positive x) =
    x


positive : Codec number (Positive number)
positive =
    Codec
        (\n ->
            if n > 0 then
                Ok (Positive n)

            else
                Err (CodecError "Must be positive")
        )
        (\(Positive n) -> n)
        "Positive"


string : Codec String String
string =
    Codec
        Ok
        identity
        "String"


int : Codec String Int
int =
    Codec
        (\s ->
            case String.toInt s of
                Just n ->
                    Ok n

                Nothing ->
                    Err (CodecError "Must be a number")
        )
        String.fromInt
        "Number"
