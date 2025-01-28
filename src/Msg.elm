module Msg exposing (Msg(..))

import Data.Answer exposing (Answer)
import Data.Question exposing (Question)


type Msg
    = NoOp
    | LoadUserGroup
    | AnswerQuestion Question Answer
    | Submit
