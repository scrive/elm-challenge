module SharedTypesTests exposing (..)

import Expect
import Json.Decode as Decode
import SharedTypes exposing (..)
import Test exposing (..)



-- Test decoding UserGroup


userGroupDecoderTest : Test
userGroupDecoderTest =
    describe "UserGroup Decoder"
        [ test "Decoding a valid UserGroup" <|
            \() ->
                let
                    sampleJson =
                        """
                        {
                            "settings": {
                                "inherited_from": null,
                                "data_retention_policy": {
                                    "idle_doc_timeout_preparation": 7,
                                    "idle_doc_timeout_closed": null,
                                    "idle_doc_timeout_canceled": 30,
                                    "idle_doc_timeout_timedout": 15,
                                    "idle_doc_timeout_rejected": null,
                                    "idle_doc_timeout_error": 3,
                                    "immediate_trash": false
                                }
                            },
                            "id": "123",
                            "parent_id": "456",
                            "name": "Group1",
                            "children": [
                                {
                                    "name": "Child1",
                                    "id": "789"
                                }
                            ]
                        }
                        """
                in
                case Decode.decodeString userGroupDecoder sampleJson of
                    Ok userGroup ->
                        Expect.equal userGroup.id "123"

                    Err err ->
                        Expect.fail <| "Decoding failed: " ++ Debug.toString err
        ]
