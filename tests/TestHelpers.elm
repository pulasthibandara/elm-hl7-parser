module TestHelpers exposing (..)

import Expect exposing (Expectation, fail)
import Parser exposing (Parser)

expectOkResult : Parser a -> String  -> (a -> Expectation) -> Expectation
expectOkResult parser source expectation =
    case Parser.run parser source of
        Result.Ok a ->
            expectation a
        Result.Err err ->
            fail <| "failed to parse with error: " ++
                (List.head err
                    |> Maybe.map (.problem >> Debug.toString)
                    |> Maybe.withDefault ""
                )

expectErrResult: Parser a -> String -> (Parser.Problem -> Expectation) -> Expectation
expectErrResult parser source expectation =
    case Parser.run parser source of
        Result.Ok a ->
            fail "successfully parsed instead of failing"
        Result.Err err ->
            List.head err
                |> Maybe.map .problem
                |> Maybe.map expectation
                |> Maybe.withDefault (fail "failed but error list is empty")
