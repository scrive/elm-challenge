module Views.Errors exposing (view)

import Data.Form exposing (ReplyValidation(..))
import Html exposing (Attribute, Html, div, text)
import Msg exposing (Msg)
import Utils exposing (classList)


view : ReplyValidation -> Html Msg
view validation =
    div [] <|
        case validation of
            Valid ->
                []

            NotValidated ->
                []

            Invalid errors ->
                List.map
                    (text
                        >> List.singleton
                        >> div [ stylesError ]
                    )
                    errors



-- Styles


stylesError : Attribute Msg
stylesError =
    classList
        [ "text-sm"
        , "text-red-600"
        , "font-medium"
        ]
