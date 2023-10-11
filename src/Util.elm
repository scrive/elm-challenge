module Util exposing (buildErrorMessage, emailToString, parseEmail, parsePhoneNumber, phoneToString)

import Http
import Parser as P exposing ((|.), (|=), Parser, Step)


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



{-
   - Email parser
   -- valid email should contain valid parts, separated by "@", "."
   -- SplitEmail = BeforeEt "dooshanstevanovic" | AfterEr "gmail" | AfterDot "com"
-}


type Email
    = Email String


type SplitEmail
    = BeforeEt String
    | AfterEt String
    | AfterDot String


type alias ConstructEmail =
    { beforeEt : String, afterEt : String, afterDot : String }


beforeEtParser : Parser SplitEmail
beforeEtParser =
    P.succeed BeforeEt
        --|= (P.chompUntilEndOr "@"
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\beforeEt ->
                        let
                            _ =
                                Debug.log "beforeEt" beforeEt
                        in
                        if String.isEmpty beforeEt || not (isLowerCase beforeEt) then
                            --P.problem "Email does not contain username"
                            P.problem "Invalid email"

                        else
                            P.succeed beforeEt
                    )
           )


afterEtParser : Parser SplitEmail
afterEtParser =
    P.succeed AfterEt
        --|= (P.chompUntilEndOr "."
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterEt ->
                        let
                            _ =
                                Debug.log "afterEt" afterEt
                        in
                        if String.isEmpty afterEt || not (isLowerCase afterEt) then
                            --P.problem "Email does not contain mail server"
                            P.problem "Invalid email"

                        else
                            P.succeed afterEt
                    )
           )


afterDotParser : Parser SplitEmail
afterDotParser =
    P.succeed AfterDot
        --|= (P.chompWhile (\c -> c /= ' ')
        |= (P.chompWhile Char.isAlphaNum
                |> P.getChompedString
                |> P.andThen
                    (\afterDot ->
                        let
                            _ =
                                Debug.log "afterDot" afterDot
                        in
                        if String.isEmpty afterDot || not (isLowerCase afterDot) then
                            -- P.problem "Email does not contain domain"
                            P.problem "Invalid email"

                        else
                            P.succeed afterDot
                    )
           )


emailParser : Parser (Maybe ConstructEmail)
emailParser =
    P.succeed
        -- (\(BeforeEt beforeEt) (AfterEt afterEt) (AfterDot afterDot) ->
        --     ConstructEmail beforeEt afterEt afterDot
        -- )
        (\a b c ->
            case ( a, b, c ) of
                ( BeforeEt be, AfterEt ae, AfterDot ad ) ->
                    Just <| ConstructEmail be ae ad

                _ ->
                    Nothing
        )
        |= beforeEtParser
        |. P.symbol "@"
        |= afterEtParser
        |. P.symbol "."
        |= afterDotParser


parseEmail : String -> Result String Email
parseEmail email =
    case P.run emailParser email of
        Err err ->
            let
                _ =
                    Debug.log "Dusan" err
            in
            Err "Invalid email"

        Ok _ ->
            Ok (Email email)



{-
   - Phone parser
   -- "+" sign on beggining
   -- "123456789101" 12 digits string after
-}


type Phone
    = Phone String


type SplitPhone
    = Plus


plusParser : Parser SplitPhone
plusParser =
    P.succeed Plus
        |. P.symbol "+"


phoneNumberStr : Parser String
phoneNumberStr =
    P.loop [] phoneHelp |> P.map String.concat


phoneHelp : List String -> Parser (Step (List String) (List String))
phoneHelp nums =
    let
        checkNum numSoFar num =
            if String.length num < 11 then
                P.Loop (num :: numSoFar)

            else
                P.Done (List.reverse numSoFar)
    in
    P.succeed (checkNum nums)
        |= (P.getChompedString <| P.chompWhile Char.isDigit)


parsePlusAndNumber : Parser Phone
parsePlusAndNumber =
    P.succeed Phone
        |. plusParser
        |= phoneNumberStr


parsePhoneNumber : String -> Result String Phone
parsePhoneNumber str =
    case P.run parsePlusAndNumber str of
        Err _ ->
            Err "Wrong phone number"

        Ok _ ->
            Ok (Phone str)


isLowerCase : String -> Bool
isLowerCase str =
    String.all Char.isLower str


emailToString : Email -> String
emailToString (Email validEmail) =
    validEmail


phoneToString : Phone -> String
phoneToString (Phone validPhone) =
    validPhone
