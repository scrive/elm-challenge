module Lenses exposing (userGroupOfModel)

import Data.UserGroup exposing (UserGroup)
import Model exposing (Model)
import Monocle.Lens exposing (Lens)


userGroupOfModel : Lens Model (Maybe UserGroup)
userGroupOfModel =
    Lens .userGroup (\b a -> { a | userGroup = b })
