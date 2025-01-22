module IdleDocTimeout exposing (IdleDocTimeout(..), all, id, title, visibleOptions)

import DataRetentionPolicy exposing (DataRetentionPolicy)
import Set exposing (Set)


type IdleDocTimeout
    = Preparation
    | Closed
    | Canceled
    | Timedout
    | Rejected
    | Error


id : IdleDocTimeout -> String
id policy =
    case policy of
        Preparation ->
            "idle_doc_timeout_preparation"

        Closed ->
            "idle_doc_timeout_closed"

        Canceled ->
            "idle_doc_timeout_canceled"

        Timedout ->
            "idle_doc_timeout_timedout"

        Rejected ->
            "idle_doc_timeout_rejected"

        Error ->
            "idle_doc_timeout_error"


title : IdleDocTimeout -> String
title policy =
    case policy of
        Preparation ->
            "Preparation"

        Closed ->
            "Closed"

        Canceled ->
            "Canceled"

        Timedout ->
            "Timedout"

        Rejected ->
            "Rejected"

        Error ->
            "Error"


all : List IdleDocTimeout
all =
    [ Preparation, Closed, Canceled, Timedout, Rejected, Error ]


visibleOptions : DataRetentionPolicy -> Set String
visibleOptions policy =
    [ policy.idleDocTimeoutPreparation |> Maybe.map (\_ -> Preparation)
    , policy.idleDocTimeoutClosed |> Maybe.map (\_ -> Closed)
    , policy.idleDocTimeoutCanceled |> Maybe.map (\_ -> Canceled)
    , policy.idleDocTimeoutTimedout |> Maybe.map (\_ -> Timedout)
    , policy.idleDocTimeoutRejected |> Maybe.map (\_ -> Rejected)
    , policy.idleDocTimeoutError |> Maybe.map (\_ -> Error)
    ]
        |> List.filterMap identity
        |> List.map id
        |> Set.fromList
