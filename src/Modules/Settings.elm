module Modules.Settings exposing
    ( DataRetentionPolicy
    , Model
    , Msg
    , Settings
    , activePolicies
    , decoder
    , empty
    , initialModel
    , policyToString
    , update
    , view
    )

import Browser.Dom as Dom
import Html exposing (Html)
import Html.Attributes as Attrs
import Html.Events as Events
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Task


type alias Model =
    { idleDocTimeOutPreparation : Maybe Int
    , idleDocTimeOutClosed : Maybe Int
    , idleDocTimeOutCancelled : Maybe Int
    , idleDocTimeOutTimedOut : Maybe Int
    , idleDocTimeOutRejected : Maybe Int
    , idleDocTimeOutError : Maybe Int
    , immediateTrash : Bool
    , isInherited : Bool
    }


type alias Settings =
    { inheritedFrom : String
    , dataRetentionPolicy : DataRetentionPolicy
    }


empty : Settings
empty =
    { inheritedFrom = ""
    , dataRetentionPolicy = emptyDataRetentionPolicy
    }


decoder : Decode.Decoder Settings
decoder =
    Decode.succeed Settings
        |> Decode.optional "inherited_from" Decode.string ""
        |> Decode.required "data_retention_policy" dataRetentionPolicyDecoder


type alias DataRetentionPolicy =
    { idleDocTimeOutPreparation : Maybe Int
    , idleDocTimeOutClosed : Maybe Int
    , idleDocTimeOutCancelled : Maybe Int
    , idleDocTimeOutTimedOut : Maybe Int
    , idleDocTimeOutRejected : Maybe Int
    , idleDocTimeOutError : Maybe Int
    , immediateTrash : Bool
    }


emptyDataRetentionPolicy : DataRetentionPolicy
emptyDataRetentionPolicy =
    { idleDocTimeOutPreparation = Nothing
    , idleDocTimeOutClosed = Nothing
    , idleDocTimeOutCancelled = Nothing
    , idleDocTimeOutTimedOut = Nothing
    , idleDocTimeOutRejected = Nothing
    , idleDocTimeOutError = Nothing
    , immediateTrash = False
    }


dataRetentionPolicyDecoder : Decode.Decoder DataRetentionPolicy
dataRetentionPolicyDecoder =
    Decode.succeed DataRetentionPolicy
        |> Decode.optional "idle_doc_timeout_preparation" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_closed" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_canceled" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_timedout" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_rejected" (Decode.map Just Decode.int) Nothing
        |> Decode.optional "idle_doc_timeout_error" (Decode.map Just Decode.int) Nothing
        |> Decode.required "immediate_trash" Decode.bool


activePolicies :
    { a
        | idleDocTimeOutPreparation : Maybe Int
        , idleDocTimeOutClosed : Maybe Int
        , idleDocTimeOutCancelled : Maybe Int
        , idleDocTimeOutTimedOut : Maybe Int
        , idleDocTimeOutRejected : Maybe Int
        , idleDocTimeOutError : Maybe Int
    }
    -> List ( Policy, Int )
activePolicies policies =
    [ policies.idleDocTimeOutPreparation |> Maybe.map (\value -> ( Preparation, value ))
    , policies.idleDocTimeOutClosed |> Maybe.map (\value -> ( Closed, value ))
    , policies.idleDocTimeOutCancelled |> Maybe.map (\value -> ( Cancelled, value ))
    , policies.idleDocTimeOutTimedOut |> Maybe.map (\value -> ( TimedOut, value ))
    , policies.idleDocTimeOutRejected |> Maybe.map (\value -> ( Rejected, value ))
    , policies.idleDocTimeOutError |> Maybe.map (\value -> ( Error, value ))
    ]
        |> List.filterMap identity


type Policy
    = Preparation
    | Closed
    | Cancelled
    | TimedOut
    | Rejected
    | Error


policyToString : Policy -> String
policyToString policy =
    case policy of
        Preparation ->
            "preparation"

        Closed ->
            "closed"

        Cancelled ->
            "cancelled"

        TimedOut ->
            "timedOut"

        Rejected ->
            "rejected"

        Error ->
            "error"


inactivePolicies :
    { a
        | idleDocTimeOutPreparation : Maybe Int
        , idleDocTimeOutClosed : Maybe Int
        , idleDocTimeOutCancelled : Maybe Int
        , idleDocTimeOutTimedOut : Maybe Int
        , idleDocTimeOutRejected : Maybe Int
        , idleDocTimeOutError : Maybe Int
    }
    -> List Policy
inactivePolicies policies =
    [ ( policies.idleDocTimeOutPreparation, Preparation )
    , ( policies.idleDocTimeOutClosed, Closed )
    , ( policies.idleDocTimeOutCancelled, Cancelled )
    , ( policies.idleDocTimeOutTimedOut, TimedOut )
    , ( policies.idleDocTimeOutRejected, Rejected )
    , ( policies.idleDocTimeOutError, Error )
    ]
        |> List.filter (Tuple.first >> (==) Nothing)
        |> List.map Tuple.second


initialModel :
    { idleDocTimeOutPreparation : Maybe Int
    , idleDocTimeOutClosed : Maybe Int
    , idleDocTimeOutCancelled : Maybe Int
    , idleDocTimeOutTimedOut : Maybe Int
    , idleDocTimeOutRejected : Maybe Int
    , idleDocTimeOutError : Maybe Int
    , immediateTrash : Bool
    }
    -> { isInherited : Bool }
    -> Model
initialModel dataRetentionPolicy { isInherited } =
    { idleDocTimeOutPreparation = dataRetentionPolicy.idleDocTimeOutPreparation
    , idleDocTimeOutClosed = dataRetentionPolicy.idleDocTimeOutClosed
    , idleDocTimeOutCancelled = dataRetentionPolicy.idleDocTimeOutCancelled
    , idleDocTimeOutTimedOut = dataRetentionPolicy.idleDocTimeOutTimedOut
    , idleDocTimeOutRejected = dataRetentionPolicy.idleDocTimeOutRejected
    , idleDocTimeOutError = dataRetentionPolicy.idleDocTimeOutError
    , immediateTrash = dataRetentionPolicy.immediateTrash
    , isInherited = isInherited
    }


type Msg
    = Submitted
    | FormClosed
    | AddPolicy Policy
    | PolicyChanged Policy String
    | ImmediateTrashChecked Bool
    | Removed Policy


update : Msg -> Model -> { onSubmit : Model -> msg, onClose : msg, onFocus : msg } -> ( Model, Cmd msg )
update msg model config =
    case msg of
        Submitted ->
            let
                newModel : Model
                newModel =
                    { model
                        | idleDocTimeOutPreparation = nonZero model.idleDocTimeOutPreparation
                        , idleDocTimeOutClosed = nonZero model.idleDocTimeOutClosed
                        , idleDocTimeOutCancelled = nonZero model.idleDocTimeOutCancelled
                        , idleDocTimeOutTimedOut = nonZero model.idleDocTimeOutTimedOut
                        , idleDocTimeOutRejected = nonZero model.idleDocTimeOutRejected
                        , idleDocTimeOutError = nonZero model.idleDocTimeOutError
                    }
            in
            ( newModel
            , Task.perform config.onSubmit (Task.succeed newModel)
            )

        FormClosed ->
            ( model
            , Task.perform (\_ -> config.onClose) (Task.succeed "")
            )

        AddPolicy policy ->
            ( changePolicy policy (Just 0) model
            , Task.attempt (\_ -> config.onFocus) (Dom.focus (policyToString policy ++ "-input"))
            )

        PolicyChanged policy value ->
            ( changePolicy policy (String.toFloat value |> Maybe.map Basics.round) model
            , Cmd.none
            )

        ImmediateTrashChecked bool ->
            ( { model | immediateTrash = bool }
            , Cmd.none
            )

        Removed policy ->
            ( changePolicy policy Nothing model
            , Cmd.none
            )


nonZero : Maybe Int -> Maybe Int
nonZero maybeInt =
    case maybeInt of
        Just 0 ->
            Nothing

        _ ->
            maybeInt


changePolicy : Policy -> Maybe Int -> Model -> Model
changePolicy policy value model =
    case policy of
        Preparation ->
            { model | idleDocTimeOutPreparation = value }

        Closed ->
            { model | idleDocTimeOutClosed = value }

        Cancelled ->
            { model | idleDocTimeOutCancelled = value }

        TimedOut ->
            { model | idleDocTimeOutTimedOut = value }

        Rejected ->
            { model | idleDocTimeOutRejected = value }

        Error ->
            { model | idleDocTimeOutError = value }


view : Model -> Html Msg
view ({ isInherited } as model) =
    Html.form
        [ Attrs.class "flex flex-col gap-4 my-2 p-2.5 w-full"
        , Attrs.class "sm:w-6/12 md:w-3/6 lg:w-2/6 border rounded"
        , Attrs.class "whitespace-nowrap text-ellipsis overflow-hidden"
        , Events.onSubmit Submitted
        ]
        ([ [ Html.h1 [ Attrs.class "text-md font-semibold text-stone-700 pl-1", Attrs.class "border-b" ]
                [ Html.text "Data retention policy" ]
           ]
         , activePolicies model
            |> List.map (viewActivePolicy isInherited)
         , [ Html.span
                [ Attrs.class "flex flex-row rounded p-2.5 justify-between"
                , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                ]
                [ Html.label [ Attrs.class "text-md font-semibold text-stone-700 pl-1" ]
                    [ Html.text "immediate trash" ]
                , Html.input
                    [ Attrs.type_ "checkbox"
                    , Attrs.class "border-stone-400 w-4"
                    , Attrs.class "border rounded px-2 py-1"
                    , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                    , Attrs.disabled isInherited
                    , Events.onCheck ImmediateTrashChecked
                    , Attrs.checked model.immediateTrash
                    ]
                    []
                ]
           ]
         , if not isInherited then
            inactivePolicies model
                |> List.map (viewInactivePolicy isInherited)

           else
            []
         , [ viewSubmitSection isInherited ]
         ]
            |> List.concat
        )


viewActivePolicy : Bool -> ( Policy, Int ) -> Html Msg
viewActivePolicy isInherited ( policy, value ) =
    Html.span
        [ Attrs.class "flex flex-row rounded p-2.5 justify-between items-center"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        [ Html.label [ Attrs.class "text-md font-semibold text-stone-700 pl-1" ]
            [ Html.text (policyToString policy ++ ":") ]
        , Html.div []
            ([ Html.input
                [ Attrs.type_ "number"
                , Attrs.id (policyToString policy ++ "-input")
                , Attrs.class "focus:outline-none border-stone-400 w-24 text-md"
                , Attrs.class "text-right text-normal text-stone-700 appearance-none"
                , Attrs.class "[appearance:textfield] [&::-webkit-outer-spin-button]:appearance-none"
                , Attrs.class "[&::-webkit-inner-spin-button]:appearance-none"
                , Attrs.class "border rounded px-2 py-1"
                , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
                , Attrs.disabled isInherited
                , Attrs.value (String.fromInt value)
                , Events.onInput (PolicyChanged policy)
                ]
                []
             ]
                ++ (if isInherited then
                        []

                    else
                        [ Html.button
                            [ Attrs.class "border border-transparent rounded px-2 py-1 my-1 bg-red-400"
                            , Attrs.class "text-white outline-black hover:text-[#d2e7f9] w-12 ml-1"
                            , Attrs.type_ "button"
                            , Events.onClick (Removed policy)
                            ]
                            [ Html.text "x" ]
                        ]
                   )
            )
        ]


viewInactivePolicy : Bool -> Policy -> Html Msg
viewInactivePolicy isInherited policy =
    Html.span
        [ Attrs.class "flex flex-row rounded p-2.5 justify-between items-center bg-stone-100"
        , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
        ]
        [ Html.label [ Attrs.class "text-md font-semibold text-stone-700 pl-1" ]
            [ Html.text (policyToString policy ++ ":") ]
        , Html.button
            [ Attrs.type_ "button"
            , Attrs.class "hover:bg-[#d2e7f9] border-stone-400 text-md"
            , Attrs.class "text-right text-normal text-stone-700"
            , Attrs.class "border rounded px-2 py-1"
            , Attrs.classList [ ( "bg-[#e8f3fc]", isInherited ) ]
            , Attrs.disabled isInherited
            , Events.onClick (AddPolicy policy)
            ]
            [ Html.text "add" ]
        ]


viewSubmitSection : Bool -> Html Msg
viewSubmitSection isInherited =
    Html.span
        [ Attrs.class "flex flex-row gap-4"
        , Attrs.classList
            [ ( "justify-end", not isInherited )
            , ( "justify-center", isInherited )
            ]
        ]
        (if isInherited then
            [ Html.button
                [ Attrs.class "w-2/6"
                , Attrs.class "border border-black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                , Attrs.type_ "button"
                , Events.onClick FormClosed
                ]
                [ Html.text "close" ]
            ]

         else
            [ Html.button
                [ Attrs.class "border border-black rounded px-2 py-1 text-black hover:bg-[#d2e7f9]"
                , Attrs.type_ "button"
                , Events.onClick FormClosed
                ]
                [ Html.text "cancel" ]
            , Html.button
                [ Attrs.class "border border-transparent rounded px-2 py-1 bg-[#1e88e2]"
                , Attrs.class "text-white outline-black hover:text-[#d2e7f9]"
                , Attrs.type_ "submit"
                , Events.onClick Submitted
                ]
                [ Html.text "apply" ]
            ]
        )
