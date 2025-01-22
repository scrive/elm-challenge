module Form.Error exposing (..)


type Field
    = NewTagName
    | NewTagValue


type Error = Error ( Field, String )
