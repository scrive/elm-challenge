module TaskSettings exposing (Model, Msg, init, update, view)

import Common.FormField exposing (FormField, initFormFieldRaw, updateFormField)
import Common.Validation as Validation exposing (Positive(..), getPositive)
import Dict exposing (Dict)
import Element exposing (Element, centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, minimum, padding, paddingXY, px, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Palette
import Result.Extra
import Types.UserGroup exposing (DataRetentionPolicyDto, SettingsDto, UserGroupId)


type DataRetentionType
    = Preparation
    | Closed
    | Canceled
    | TimedOut
    | Rejected
    | Error


allDataRetentionTypes : List DataRetentionType
allDataRetentionTypes =
    [ Preparation, Closed, Canceled, TimedOut, Rejected, Error ]



-- for showing the data retention type in the UI


showDataRetentionType : DataRetentionType -> String
showDataRetentionType dataRetentionType =
    case dataRetentionType of
        Preparation ->
            "Preparation"

        Closed ->
            "Closed"

        Canceled ->
            "Canceled"

        TimedOut ->
            "Timed Out"

        Rejected ->
            "Rejected"

        Error ->
            "Error"



-- for converting the data retention type to an integer for use as a key in a Dict


dataRetentionTypeToInt : DataRetentionType -> Int
dataRetentionTypeToInt dataRetentionType =
    case dataRetentionType of
        Preparation ->
            0

        Closed ->
            1

        Canceled ->
            2

        TimedOut ->
            3

        Rejected ->
            4

        Error ->
            5


lookupDataRetentionValue : Dict Int a -> DataRetentionType -> Maybe ( DataRetentionType, a )
lookupDataRetentionValue dataRetention dataRetentionType =
    Dict.get (dataRetentionTypeToInt dataRetentionType) dataRetention
        |> Maybe.map (Tuple.pair dataRetentionType)


collectRetentionTypes : Bool -> (a -> Int) -> List ( DataRetentionType, a ) -> DataRetentionPolicyDto
collectRetentionTypes immediateTrash mapValue =
    List.map (Tuple.mapSecond mapValue)
        >> List.foldl
            (\( drt, value ) acc ->
                case drt of
                    Preparation ->
                        { acc | preparation = Just value }

                    Closed ->
                        { acc | closed = Just value }

                    Canceled ->
                        { acc | canceled = Just value }

                    TimedOut ->
                        { acc | timedOut = Just value }

                    Rejected ->
                        { acc | rejected = Just value }

                    Error ->
                        { acc | error = Just value }
            )
            { preparation = Nothing
            , closed = Nothing
            , canceled = Nothing
            , timedOut = Nothing
            , rejected = Nothing
            , error = Nothing
            , immediateTrash = immediateTrash
            }


getDataRetentionValueFromDto : DataRetentionType -> DataRetentionPolicyDto -> Maybe Int
getDataRetentionValueFromDto dataRetentionType dataRetention =
    case dataRetentionType of
        Preparation ->
            dataRetention.preparation

        Closed ->
            dataRetention.closed

        Canceled ->
            dataRetention.canceled

        TimedOut ->
            dataRetention.timedOut

        Rejected ->
            dataRetention.rejected

        Error ->
            dataRetention.error


type alias DataRetentionField =
    FormField String (Positive Int)


type alias InheritedSettings =
    { inheritedFrom : UserGroupId
    , dataRetention : Dict Int Int
    , immediateTrash : Bool
    }


type alias OwnSettings =
    { dataRetention : Dict Int DataRetentionField
    , immediateTrash : Bool
    , newDataRetentionTypes : Maybe (List DataRetentionType)
    }


type Model
    = Own OwnSettings
    | Inherited InheritedSettings


withUpdatedDataRetentionValue : DataRetentionType -> String -> OwnSettings -> OwnSettings
withUpdatedDataRetentionValue dataRetentionType value settings =
    { settings
        | dataRetention =
            Dict.update
                (dataRetentionTypeToInt dataRetentionType)
                (Maybe.map (updateFormField value))
                settings.dataRetention
    }


withoutDataRetentionValue : DataRetentionType -> OwnSettings -> OwnSettings
withoutDataRetentionValue dataRetentionType settings =
    { settings
        | dataRetention =
            Dict.remove (dataRetentionTypeToInt dataRetentionType) settings.dataRetention
    }


withNewDataRetentionValue : DataRetentionType -> OwnSettings -> OwnSettings
withNewDataRetentionValue dataRetentionType settings =
    { settings
        | dataRetention =
            Dict.insert
                (dataRetentionTypeToInt dataRetentionType)
                (initFormFieldRaw dataRetentionValueCodec "")
                settings.dataRetention
        , newDataRetentionTypes = Nothing
    }


dataRetentionValueCodec : Validation.Codec String (Positive Int)
dataRetentionValueCodec =
    Validation.int |> Validation.compose Validation.positive


constructSettingsDto : Maybe UserGroupId -> DataRetentionPolicyDto -> SettingsDto
constructSettingsDto inheritedFrom dataRetention =
    { inheritedFrom = inheritedFrom
    , dataRetentionPolicy = dataRetention
    }


fromDto : SettingsDto -> Model
fromDto settingsDto =
    case settingsDto.inheritedFrom of
        Just inheritedFrom ->
            Inherited
                { inheritedFrom = inheritedFrom
                , immediateTrash = settingsDto.dataRetentionPolicy.immediateTrash
                , dataRetention =
                    allDataRetentionTypes
                        |> List.filterMap
                            (\drt ->
                                getDataRetentionValueFromDto drt settingsDto.dataRetentionPolicy
                                    |> Maybe.map (\v -> ( dataRetentionTypeToInt drt, v ))
                            )
                        |> Dict.fromList
                }

        Nothing ->
            Own
                { dataRetention =
                    allDataRetentionTypes
                        |> List.filterMap
                            (\drt ->
                                getDataRetentionValueFromDto drt settingsDto.dataRetentionPolicy
                                    |> Maybe.map (\v -> ( dataRetentionTypeToInt drt, initFormFieldRaw dataRetentionValueCodec (String.fromInt v) ))
                            )
                        |> Dict.fromList
                , immediateTrash = settingsDto.dataRetentionPolicy.immediateTrash
                , newDataRetentionTypes = Nothing
                }


toDto : Model -> Result Validation.CodecError SettingsDto
toDto settings =
    case settings of
        Inherited inheritedSettings ->
            allDataRetentionTypes
                |> List.filterMap (lookupDataRetentionValue inheritedSettings.dataRetention)
                |> collectRetentionTypes inheritedSettings.immediateTrash identity
                |> constructSettingsDto (Just inheritedSettings.inheritedFrom)
                |> Ok

        Own ownSettings ->
            allDataRetentionTypes
                |> List.filterMap (lookupDataRetentionValue ownSettings.dataRetention)
                |> Result.Extra.combineMap (\( drt, field ) -> field.val |> Result.map (Tuple.pair drt))
                |> Result.map (collectRetentionTypes ownSettings.immediateTrash getPositive >> constructSettingsDto Nothing)


type Msg
    = DataRetentionValueChanged DataRetentionType String
    | DataRetentionValueAdded DataRetentionType
    | DataRetentionValueRemoved DataRetentionType
    | ShowAvailableRetentionTypes (List DataRetentionType)
    | HideAvailableRetentionTypes
    | ImmediateTrashChanged Bool



-- Init


init : SettingsDto -> Model
init =
    fromDto


update : Msg -> Model -> Model
update msg model =
    case ( msg, model ) of
        ( DataRetentionValueChanged dataRetentionType value, Own settings ) ->
            settings |> withUpdatedDataRetentionValue dataRetentionType value |> Own

        ( DataRetentionValueAdded dataRetentionType, Own settings ) ->
            settings |> withNewDataRetentionValue dataRetentionType |> Own

        ( DataRetentionValueRemoved dataRetentionType, Own settings ) ->
            settings |> withoutDataRetentionValue dataRetentionType |> Own

        ( ShowAvailableRetentionTypes dataRetentionTypes, Own settings ) ->
            { settings | newDataRetentionTypes = Just dataRetentionTypes } |> Own

        ( HideAvailableRetentionTypes, Own settings ) ->
            { settings | newDataRetentionTypes = Nothing } |> Own

        ( ImmediateTrashChanged checked, Own settings ) ->
            { settings | immediateTrash = checked } |> Own

        ( ImmediateTrashChanged _, Inherited _ ) ->
            model

        ( DataRetentionValueAdded _, Inherited _ ) ->
            model

        ( DataRetentionValueRemoved _, Inherited _ ) ->
            model

        ( ShowAvailableRetentionTypes _, Inherited _ ) ->
            model

        ( DataRetentionValueChanged _ _, Inherited _ ) ->
            model

        ( HideAvailableRetentionTypes, Inherited _ ) ->
            model


view : { mapMsg : Msg -> msg, onCancel : msg, onSubmit : SettingsDto -> msg } -> Model -> Element msg
view { mapMsg, onCancel, onSubmit } model =
    let
        settingsDtoResult =
            toDto model

        saveButtonColor =
            case settingsDtoResult of
                Ok _ ->
                    Palette.ok

                Err _ ->
                    Palette.disabled

        dataRetentionPolicyItems : List (Element Msg)
        dataRetentionPolicyItems =
            case model of
                Own settings ->
                    ownSettingsView settings

                Inherited settings ->
                    inheritedSettingsView settings

        buttons : List (Element msg)
        buttons =
            case model of
                Own _ ->
                    [ Input.button
                        [ height <| px 50
                        , width fill
                        , Border.rounded 7
                        , Border.width 1
                        , Border.color Palette.primary
                        , Background.color Palette.backgroundLight
                        , Font.color Palette.text
                        ]
                        { onPress = Just onCancel, label = el [ centerX, padding 10 ] (text "Cancel") }
                    , Input.button
                        [ height <| px 50
                        , width fill
                        , Border.rounded 7
                        , Background.color saveButtonColor
                        , Font.color Palette.textLight
                        ]
                        { onPress = settingsDtoResult |> Result.toMaybe |> Maybe.map onSubmit, label = el [ centerX, padding 10 ] (text "Save") }
                    ]

                Inherited _ ->
                    [ Input.button
                        [ height <| px 50
                        , width fill
                        , Border.rounded 7
                        , Border.width 1
                        , Border.color Palette.primary
                        , Background.color Palette.backgroundLight
                        , Font.color Palette.text
                        ]
                        { onPress = Just onCancel, label = el [ centerX, padding 10 ] (text "Back") }
                    ]

        headerText =
            case model of
                Own _ ->
                    text "Edit Settings"

                Inherited _ ->
                    text "Settings"
    in
    el
        [ width fill, height fill ]
    <|
        column
            [ width <| minimum 400 shrink
            , height <| minimum 400 shrink
            , centerX
            , centerY
            , spacing 20
            , htmlAttribute (Html.Attributes.attribute "role" "form")
            , htmlAttribute (Html.Attributes.attribute "aria-label" "Task Settings Form")
            ]
            [ el [ centerX, Font.size 42, Font.bold, Font.color Palette.text ] headerText
            , Element.map mapMsg <|
                column
                    [ padding 20
                    , spacing 10
                    , width fill
                    , Background.color Palette.background
                    , Border.rounded 7
                    ]
                    dataRetentionPolicyItems
            , row [ width fill, spacing 10 ] buttons
            ]


ownSettingsView : OwnSettings -> List (Element Msg)
ownSettingsView settings =
    let
        availableDataRetentionTypes : List DataRetentionType
        availableDataRetentionTypes =
            allDataRetentionTypes
                |> List.filter (\drt -> not <| Dict.member (dataRetentionTypeToInt drt) settings.dataRetention)

        selectedRetentionTypes : List (Element Msg)
        selectedRetentionTypes =
            selectedDataRetentionValues settings |> List.map dataRetentionValueView

        newRetentionTypes : Maybe (Element Msg)
        newRetentionTypes =
            settings.newDataRetentionTypes |> Maybe.map dataRetentionTypesSelection

        addRetentionTypeButton : Maybe (Element Msg)
        addRetentionTypeButton =
            case availableDataRetentionTypes of
                [] ->
                    Nothing

                drts ->
                    Just <|
                        selectionButton
                            { onPress =
                                case settings.newDataRetentionTypes of
                                    Nothing ->
                                        Just <| ShowAvailableRetentionTypes drts

                                    Just _ ->
                                        Just HideAvailableRetentionTypes
                            , label = el [ centerX ] (text "Add data retention policy")
                            }

        immediateTrashCheckbox : Bool -> Element Msg
        immediateTrashCheckbox value =
            Input.checkbox
                [ centerX ]
                { icon = Input.defaultCheckbox
                , checked = value
                , label = Input.labelRight [] (text "Immediate trash")
                , onChange = ImmediateTrashChanged
                }
    in
    selectedRetentionTypes
        ++ List.filterMap identity [ addRetentionTypeButton, newRetentionTypes ]
        ++ [ hr, immediateTrashCheckbox settings.immediateTrash ]


inheritedSettingsView : InheritedSettings -> List (Element Msg)
inheritedSettingsView settings =
    let
        immediateTrash : Element Msg
        immediateTrash =
            row [ centerX, width fill ] <|
                [ el [ width (fillPortion 1), Font.alignRight ] <| text "Immediate trash: "
                , el [ width (fillPortion 1), Font.alignLeft ] <|
                    text <|
                        if settings.immediateTrash then
                            "True"

                        else
                            "False"
                ]
    in
    (allDataRetentionTypes
        |> List.filterMap (lookupDataRetentionValue settings.dataRetention)
        |> List.map
            (\( drt, value ) ->
                row
                    [ spacing 10, width fill ]
                    [ el [ width (fillPortion 1), Font.alignRight ] <| text <| showDataRetentionType drt
                    , el [ width (fillPortion 1), Font.alignLeft ] <| text <| String.fromInt value
                    ]
            )
    )
        ++ [ hr, immediateTrash ]


dataRetentionTypesSelection : List DataRetentionType -> Element Msg
dataRetentionTypesSelection dataRetentionTypes =
    column
        [ spacing 5, width fill ]
        (List.map
            (\drt ->
                selectionButton
                    { onPress = Just <| DataRetentionValueAdded drt
                    , label = el [ centerX ] <| text <| showDataRetentionType drt
                    }
            )
            dataRetentionTypes
        )


selectedDataRetentionValues : OwnSettings -> List ( DataRetentionType, DataRetentionField )
selectedDataRetentionValues settings =
    allDataRetentionTypes
        |> List.filterMap (\drt -> Dict.get (dataRetentionTypeToInt drt) settings.dataRetention |> Maybe.map (\value -> ( drt, value )))


actionButton : { onPress : Maybe msg, label : Element msg } -> Element msg
actionButton =
    Input.button
        [ height <| px 50
        , width <| minimum 50 shrink
        , Border.rounded 7
        , Background.color Palette.primary
        , Font.color Palette.textLight
        ]


selectionButton : { onPress : Maybe msg, label : Element msg } -> Element msg
selectionButton =
    Input.button
        [ height <| px 50
        , width fill
        , Border.rounded 7
        , Background.color Palette.primary
        , Font.color Palette.textLight
        ]


hr : Element msg
hr =
    el
        [ width fill
        , paddingXY 0 5
        ]
    <|
        Element.html <|
            Html.hr [] []


dataRetentionValueView : ( DataRetentionType, DataRetentionField ) -> Element Msg
dataRetentionValueView ( drt, field ) =
    let
        accentColor =
            case field.val of
                Err _ ->
                    Palette.error

                Ok _ ->
                    Palette.text
    in
    row
        [ spacing 5, width fill ]
        [ Input.text
            [ height <| px 50
            , width <| fillPortion 1
            , Border.rounded 7
            , Border.width 1
            , Border.color accentColor
            , htmlAttribute (Html.Attributes.type_ "number")
            , htmlAttribute <|
                Html.Attributes.attribute "aria-invalid" <|
                    if Result.Extra.isErr field.val then
                        "true"

                    else
                        "false"
            ]
            { onChange = DataRetentionValueChanged drt
            , text = field.raw
            , placeholder = Nothing
            , label =
                Input.labelLeft
                    [ Font.color accentColor
                    , padding 5
                    , width <| fillPortion 1
                    ]
                    (text <| showDataRetentionType drt)
            }
        , actionButton
            { onPress = Just <| DataRetentionValueRemoved drt
            , label = el [ centerX ] (text "✕")
            }
        ]
