module Main exposing (main)

import Browser
import Model exposing (Model)
import Msg exposing (Msg(..))
import Update exposing (update)
import Update.Extra exposing (andThen)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.application
        { view =
            \model ->
                { title = "Scrive elm challenge task"
                , body = [ view model ]
                }
        , init = \_ _ _ -> init
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always NoOp
        , onUrlChange = always NoOp
        }


init : ( Model, Cmd Msg )
init =
    ( Model.init, Cmd.none )
        |> andThen update LoadUserGroup
