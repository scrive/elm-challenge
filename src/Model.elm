module Model exposing (Model, formOfModel, init, userGroupOfModel)

import AssocList
import Data.Form exposing (Form, ReplyValidation(..))
import Data.UserGroup exposing (UserGroup)
import Monocle.Lens exposing (Lens)


type alias Model =
    { userGroup : Maybe UserGroup
    , form : Form
    }


init : Model
init =
    { userGroup = Nothing
    , form = ( AssocList.empty, NotValidated )
    }


userGroupOfModel : Lens Model (Maybe UserGroup)
userGroupOfModel =
    Lens .userGroup (\b a -> { a | userGroup = b })


formOfModel : Lens Model Form
formOfModel =
    Lens .form (\b a -> { a | form = b })
