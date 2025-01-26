module UserGroup exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int)
import Json.Decode as Decode
import Json.Encode as Encode
import Test exposing (..)
import Types.UserGroup exposing (..)


userGroupIdFuzzer : Fuzzer UserGroupId
userGroupIdFuzzer =
    int
        |> Fuzz.map UserGroupId


childUserGroupDtoFuzzer : Fuzzer ChildUserGroupDto
childUserGroupDtoFuzzer =
    Fuzz.map2
        ChildUserGroupDto
        userGroupIdFuzzer
        Fuzz.string


dataRetentionPolicyDtoFuzzer : Fuzzer DataRetentionPolicyDto
dataRetentionPolicyDtoFuzzer =
    Fuzz.map7
        DataRetentionPolicyDto
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe Fuzz.int)
        (Fuzz.maybe Fuzz.int)
        Fuzz.bool


settingsDtoFuzzer : Fuzzer SettingsDto
settingsDtoFuzzer =
    Fuzz.map2
        SettingsDto
        (Fuzz.maybe userGroupIdFuzzer)
        dataRetentionPolicyDtoFuzzer


preferredContactMethodFuzzer : Fuzzer PreferredContactMethod
preferredContactMethodFuzzer =
    Fuzz.oneOf
        [ Fuzz.constant Email
        , Fuzz.constant Phone
        , Fuzz.constant Post
        ]


addressDtoFuzzer : Fuzzer AddressDto
addressDtoFuzzer =
    Fuzz.map8
        AddressDto
        preferredContactMethodFuzzer
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)
        (Fuzz.maybe Fuzz.string)


contactDetailsDtoFuzzer : Fuzzer ContactDetailsDto
contactDetailsDtoFuzzer =
    Fuzz.map2
        ContactDetailsDto
        (Fuzz.maybe userGroupIdFuzzer)
        addressDtoFuzzer


tagDtoFuzzer : Fuzzer TagDto
tagDtoFuzzer =
    Fuzz.map2
        TagDto
        Fuzz.string
        (Fuzz.maybe Fuzz.string)


userGroupDtoFuzzer : Fuzzer UserGroupDto
userGroupDtoFuzzer =
    Fuzz.map7
        UserGroupDto
        userGroupIdFuzzer
        (Fuzz.maybe userGroupIdFuzzer)
        Fuzz.string
        (Fuzz.list childUserGroupDtoFuzzer)
        settingsDtoFuzzer
        contactDetailsDtoFuzzer
        (Fuzz.list tagDtoFuzzer)


encodeDecodeRoundtrip : Test
encodeDecodeRoundtrip =
    describe "UserGroup encode/decode " <|
        [ fuzz userGroupDtoFuzzer "roundtrip test" <|
            \userGroupDto ->
                let
                    encoded =
                        encodeUserGroupDto userGroupDto |> Encode.encode 2

                    decoded =
                        Decode.decodeString decodeUserGroupDto encoded
                in
                Expect.equal
                    decoded
                    (Ok userGroupDto)
        ]
