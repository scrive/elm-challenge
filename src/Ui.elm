module Ui exposing
  ( textInputDisabled
  , textInput, TextInputConfig
  , select, SelectConfig
  )


import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Element as E exposing (Element)
import Element.Input as I
import Element.Font as F
import Element.Border as B
import Element.Events as EE
import Element.Background as BG
import Ui.Theme as T
import FeatherIcons as I
import Json.Decode as D



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
          , E.height (E.px 40)
          , B.width 1
          , B.rounded T.space1
          , B.color T.gray200
          , BG.color T.gray50
          , F.color T.gray600
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



-- SELECT


type alias SelectConfig a msg =
  { label : Element msg
  , selected : a
  , options : List a
  , toOptionLabel : a -> Element msg
  , isOpen : Bool
  , onOpen : Bool -> msg
  , onSelect : a -> msg
  }


select : SelectConfig a msg -> Element msg
select config =
  E.el [ E.below (if config.isOpen then (selectDropdown config) else E.none) ] <|
    I.button
        [ E.htmlAttribute (HE.stopPropagationOn "click" (D.succeed (config.onOpen (not config.isOpen), True))) ]
        { onPress = Just (config.onOpen (not config.isOpen))
        , label = selectAnchor config
        }


selectAnchor : SelectConfig a msg -> Element msg
selectAnchor config =
  let viewInput =
        E.row
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

      viewLabel =
        E.el
          [ F.size T.fontSize2
          , F.color T.gray700
          , E.width E.fill
          ]
          config.label

      viewChevron =
        E.el [ F.color T.gray500, E.alignRight ] <|
          if config.isOpen
              then icon T.fontSize3 I.chevronUp
              else icon T.fontSize3 I.chevronDown
  in
  E.column
    [ E.spacing T.space2
    , E.width (E.minimum T.space16 E.shrink)
    ]
    [ viewLabel
    , viewInput
        [ config.toOptionLabel config.selected
        , viewChevron
        ]
    ]


selectDropdown : SelectConfig a msg -> Element msg
selectDropdown config =
  E.column
    [ BG.color T.white
    , B.width 1
    , B.rounded T.space1
    , B.color T.gray200
    , E.moveDown T.space1
    , E.width (E.minimum T.space16 E.shrink)
    , E.height (E.maximum T.space15 E.fill)
    , E.scrollbarX
    ] <|
      let viewOne option =
            I.button
              [ E.paddingXY T.space3 T.space3
              , E.width E.fill
              , F.size T.fontSize2
              , F.color T.gray900
              , E.mouseOver [ BG.color T.gray50 ]
              , E.focused [ BG.color T.blue50 ]
              ]
              { label = viewLabel option, onPress = Just (config.onSelect option) }

          viewLabel option =
            E.row
              [ E.width E.fill
              , E.spacing T.space2
              ]
              [ config.toOptionLabel option
              , E.el [ F.color T.blue500, E.alignRight ] (viewCheck option)
              ]

          viewCheck option =
            if option == config.selected
              then icon T.fontSize2 I.check
              else E.none
        in
        List.map viewOne config.options


icon : Float -> I.Icon -> Element msg
icon size icon_ =
  icon_
    |> I.withSize size
    |> I.toHtml []
    |> E.html
    |> E.el []
