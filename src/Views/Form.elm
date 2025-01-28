module Views.Form exposing (view)

import AssocList
import Data.Form exposing (Form, ReplyValidation(..), getVisibleQuestions, isFormValid, isQuestionDisabled)
import Data.Question exposing (QuestionTag(..), addressQuestions)
import Html exposing (Attribute, Html, div, text)
import Html.Events exposing (onClick)
import Html.Extra exposing (nothing)
import Maybe.Extra
import Msg exposing (Msg(..))
import UI.Button as Button
import Utils exposing (classList, ifElse)
import Views.Errors as Errors
import Views.Question as Question


view : Form -> Html Msg
view ( entries, validation ) =
    div [ stylesForm ] <|
        List.concat
            [ addressQuestions
                |> getVisibleQuestions entries
                |> List.map
                    (\question ->
                        AssocList.get question.tag entries
                            |> Maybe.Extra.unwrap nothing
                                (Question.view { disabled = isQuestionDisabled entries question } question)
                    )
            , [ Button.view { label = "Submit Form" } [ onClick Submit ] ]
            , [ ifElse (isFormValid ( entries, validation ))
                    (div [ classList [ "text-green-600" ] ] [ text "Saved!" ])
                    (Errors.view validation)
              ]
            ]



-- Styles


stylesForm : Attribute Msg
stylesForm =
    classList
        [ "grid"
        , "gap-4"
        ]
