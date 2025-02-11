module Ui exposing
  ( textInputDisabled
  , textInput, TextInputConfig
  , select, SelectConfig
  , button, ButtonConfig
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
  { label : String
  , selected : a
  , options : List a
  , toOptionLabel : a -> String
  , fromOptionLabel : String -> Maybe a
  , onSelect : a -> msg
  , id : String
  }


select : SelectConfig a msg -> Element msg
select config =
  let toPx int =
        String.fromInt int ++ "px"

      decodeOption str =
        case config.fromOptionLabel str of
          Just option ->
            D.succeed option

          Nothing ->
            D.fail "Unknown option"
  in
  E.el [ E.width (E.minimum T.space16 E.shrink) ] <| E.html <|
    H.div
      [ HA.style "display" "inline-flex"
      , HA.style "width" "100%"
      , HA.style "flex-direction" "column"
      , HA.style "gap" (toPx T.space2)
      ]
      [ H.label
          [ HA.for config.id
          , HA.style "font-size" (toPx T.fontSize2)
          , HA.style "color" "#344055"
          ]
          [ H.text config.label ]
      , H.node "style" [] [ H.text
          """ .scrive-select-container:focus-within { border: 1px solid #53b1fd; box-shadow: 0px 0px 4px #eff8ff; }
              .scrive-select:focus { outline: none; }
          """
          ]
      , H.div
          [ HA.style "border-radius" (toPx T.space1)
          , HA.style "border" "1px solid #ebecf0"
          , HA.style "width" "100%"
          , HA.class "scrive-select-container"
          ]
          [ H.select
              [ HA.id config.id
              , HA.style "width" "100%"
              , HA.class "scrive-select"
              , HA.style "border-radius" (toPx T.space1)
              , HA.style "padding" (toPx T.space2 ++ " " ++ toPx T.space3)
              , HA.style "border-right" (toPx T.space2 ++ " solid #fff")
              , HE.on "change" (HE.targetValue |> D.andThen decodeOption |> D.map config.onSelect)
              ]
              (List.map (selectOption config) config.options)
          ]
      ]


selectOption : SelectConfig a msg -> a -> Html msg
selectOption config option =
  H.option
    [ HA.value (config.toOptionLabel option)
    , HA.selected (option == config.selected)
    ]
    [ H.text (config.toOptionLabel option) ]



-- BUTTON



type alias ButtonConfig msg =
  { label : Element msg
  , onClick : Maybe msg
  }


button : ButtonConfig msg -> Element msg
button config =
  let actionAttrs =
        case config.onClick of
          Nothing ->
            [ BG.color T.blue300
            , B.color T.blue300
            , F.color T.white
            ]

          Just _ ->
            [ E.mouseOver
                [ BG.color T.blue600
                , F.color T.white
                ]
            , E.focused
                [ BG.color T.blue500
                , F.color T.white
                , B.shadow
                    { offset = ( 0, 0 )
                    , size = 4
                    , blur = 0
                    , color = T.blue200
                    }
                ]
            ]
  in
  I.button
    ([ E.paddingXY 16 10
    , BG.color T.blue500
    , F.center
    , F.size T.fontSize2
    , F.color T.white
    , B.width 1
    , B.rounded 4
    , B.color T.blue500
    ] ++ actionAttrs)
    { label = config.label
    , onPress = config.onClick
    }


-- HELPERS


icon : Float -> I.Icon -> Element msg
icon size icon_ =
  icon_
    |> I.withSize size
    |> I.toHtml []
    |> E.html
    |> E.el []