module Data.Settings exposing (..)


type DataRetentionPolicy
    = Preparation
    | Closed
    | Canceled
    | TimedOut
    | Rejected
    | Error
    | Other String


type alias PolicyList = List { policy : DataRetentionPolicy, value : Int }


type Settings
    = Settings
        { inheritedFrom : Maybe Int
        , policy : { list : PolicyList, trash : Maybe ImmediateTrash }
        }


type ImmediateTrash = ImmediateTrash