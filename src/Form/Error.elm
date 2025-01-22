module Form.Error exposing (..)


type Field
    = NewTagName
    | NewTagValue


type Error = Error ( Field, String )


make : Field -> String -> Error
make f s = Tuple.pair f s |> Error


textOf : Error -> String
textOf (Error (_, text)) = text


fieldOf : Error -> Field
fieldOf (Error (field, _)) = field