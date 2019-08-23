module TestHelpers exposing (..)

import Expect exposing (Expectation, fail)
import HL7Parser exposing (Problem(..))
import Parser.Advanced exposing (Parser, run)

expectOkResult : Parser c Problem a -> String  -> (a -> Expectation) -> Expectation
expectOkResult parser source expectation =
    case run parser source of
        Result.Ok a ->
            expectation a
        Result.Err err ->
            fail <| "failed to parse with error: " ++
                (List.head err
                    |> Maybe.map (.problem >> Debug.toString)
                    |> Maybe.withDefault ""
                )

expectErrResult: Parser c Problem a -> String -> (Problem -> Expectation) -> Expectation
expectErrResult parser source expectation =
    case run parser source of
        Result.Ok a ->
            fail "successfully parsed instead of failing"
        Result.Err err ->
            List.head err
                |> Maybe.map .problem
                |> Maybe.map expectation
                |> Maybe.withDefault (fail "failed but error list is empty")
