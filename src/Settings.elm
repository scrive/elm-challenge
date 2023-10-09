module Settings exposing (..)

import Accessibility.Aria as A11y exposing (..)
import Html exposing (Html)
import Html.Attributes as Attrs exposing (..)
import Html.Events exposing (onClick)
import Set exposing (Set)
import SharedTypes exposing (..)


viewSettingsForm : UserGroup -> Set String -> (String -> msg) -> Html msg
viewSettingsForm userGroup isInputVisible onInputShow =
    let
        -- Extract settings data from the user group
        settings =
            userGroup.settingsData

        -- Determine if the settings are inherited or not
        isInherited =
            case settings.inheritedFrom of
                Just "Null" ->
                    False

                _ ->
                    True

        -- Helper function to check if a value is present (Just) or not (Nothing)
        hasValue : Maybe Int -> Bool
        hasValue value =
            case value of
                Just _ ->
                    True

                Nothing ->
                    False

        -- Extract retention policy from settings data
        retentionPolicty =
            settings.dataRetentionPolicy

        -- Define a helper function to create input fields
        inputfield : Maybe Int -> String -> Html msg
        inputfield value fieldTitle =
            let
                -- Convert the Maybe Int value to a String
                fieldValue =
                    case value of
                        Just int ->
                            String.fromInt <| int

                        Nothing ->
                            "0"

                -- Create a label for the input field
                rowTitle =
                    Html.label [ Attrs.class "block text-sm font-medium text-gray-700 mb-4" ] [ Html.text fieldTitle ]
            in
            -- Conditionally render an input field or a button based on whether the value is present
            if hasValue value then
                Html.div []
                    [ rowTitle
                    , Html.input
                        [ Attrs.disabled (not isInherited)
                        , Attrs.class "mt-1 p-2 block w-full border border-gray-300 rounded-md focus:ring focus:ring-blue-200 focus:border-blue-300"
                        , Attrs.value fieldValue
                        , A11y.label ("input for " ++ fieldTitle)
                        ]
                        []
                    ]

            else
                Html.div []
                    [ rowTitle
                    , if Set.member fieldTitle isInputVisible then
                        Html.input
                            [ Attrs.disabled (not isInherited)
                            , Attrs.class "mt-1 p-2 block w-full border border-gray-300 rounded-md focus:ring focus:ring-blue-200 focus:border-blue-300"
                            , A11y.label ("input of " ++ fieldTitle)
                            ]
                            []

                      else
                        Html.button
                            [ Attrs.class "w-full bg-blue-500 text-white rounded-md hover:bg-blue-600 small-button p-2"
                            , onClick (onInputShow fieldTitle)
                            , A11y.label ("Button to add " ++ fieldTitle ++ " field")
                            ]
                            [ Html.text "Add Value" ]
                    ]
    in
    -- Define the overall structure of the settings form
    Html.div [ Attrs.class "mt-4 bg-white p-6" ]
        [ Html.div []
            [ Html.label
                [ Attrs.class "block text-4xl text-center font-large text-gray-700 mb-10"
                , A11y.label "Data Retention Settings"
                ]
                [ Html.text "Data Retention Settings" ]
            , Html.div [ Attrs.class "grid md:grid-cols-2 gap-4" ]
                [ inputfield retentionPolicty.idleDocTimeoutPreparation "Preperation (Days)"
                , inputfield retentionPolicty.idleDocTimeoutClosed "Closed (Days)"
                , inputfield retentionPolicty.idleDocTimeoutCanceled "Canceled (Days)"
                , inputfield retentionPolicty.idleDocTimeoutTimedout "Timed Out (Days)"
                , inputfield retentionPolicty.idleDocTimeoutRejected "Rejected (Days)"
                , inputfield retentionPolicty.idleDocTimeoutError "Error (Days)"
                , Html.div [ Attrs.class "flex items-center" ]
                    [ Html.input
                        [ type_ "checkbox"
                        , Attrs.checked retentionPolicty.immediateTrash
                        , Attrs.disabled (not isInherited)
                        , A11y.label
                            ("Checkbox for Immediate trash is "
                                ++ (if retentionPolicty.immediateTrash then
                                        "checked"

                                    else
                                        "not checked"
                                   )
                            )
                        ]
                        []
                    , Html.label [ Attrs.class "ml-2 block text-sm font-medium text-gray-700" ] [ Html.text "Immediate Trash" ]
                    ]
                ]
            , Html.button
                [ Attrs.class "mt-10 bg-blue-500 w-full text-white rounded-md hover:bg-blue-600 p-2"
                , Attrs.type_ "submit"
                , A11y.label "Button to submit"
                ]
                [ Html.text "Submit" ]
            ]
        ]
