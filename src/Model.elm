module Model exposing (Model)

import Data.UserGroup exposing (UserGroup)


type alias Model =
    { userGroup : Maybe UserGroup }
