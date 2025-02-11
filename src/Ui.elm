module Ui exposing
  ( textInputDisabled
  , textInput, TextInputConfig
  )


import Element as E exposing (Element)
import Element.Input as I
import Element.Font as F
import Element.Border as B
import Element.Events as EE
import Element.Background as BG
import Ui.Theme as T



-- TEXT INPUT


type alias TextInputConfig msg =
  { label : String
  , value : String
  , placeholder : String
  , onChange : String -> msg
  }


textInput : TextInputConfig msg -> Element msg
textInput config =
  let viewInput =
        I.text
          [ E.paddingXY T.space3 T.space3
          , E.width E.fill
          , B.width 1
          , B.rounded T.space1
          , B.color T.gray200
          , E.focused
              [ B.color T.blue400
              , B.shadow
                  { offset = ( 0, 0 )
                  , size = 4
                  , blur = 0
                  , color = T.blue50
                  }
              ]
          ]
          { onChange = config.onChange
          , text = config.value
          , placeholder = Just (I.placeholder [ F.color T.gray400, F.size T.fontSize2 ] (E.text config.placeholder))
          , label = I.labelHidden config.label
          }

      viewLabel =
        E.el
          [ F.size T.fontSize2
          , F.color T.gray600
          ]
          (E.text config.label)
  in
  E.column
    [ E.spacing T.space2
    , E.width (E.minimum T.space16 E.shrink)
    ]
    [ viewLabel
    , viewInput
    ]


textInputDisabled : String -> String -> Element msg
textInputDisabled label value =
  let viewInput =
        E.el
          [ E.paddingXY T.space3 T.space3
          , E.width E.fill
          , E.height (E.px 42)
          , B.width 1
          , B.rounded T.space1
          , B.color T.gray200
          , BG.color T.gray50
          ]
          (E.text value)

      viewLabel =
        E.el
          [ F.size T.fontSize2
          , F.color T.gray600
          ]
          (E.text label)
  in
  E.column
    [ E.spacing T.space2
    , E.width (E.minimum T.space16 E.shrink)
    ]
    [ viewLabel
    , viewInput
    ]