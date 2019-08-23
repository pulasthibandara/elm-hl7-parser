module HelpersTest exposing (..)

import Expect
import Fuzz exposing (Fuzzer, int, list, string)
import Helpers exposing (maybeToParser, parseMaybe, parseSizedNumber, parseStringSegment, parseStringSegmentOfSize)
import Parser.Advanced exposing (problem, succeed)
import Test exposing (..)
import TestHelpers exposing (expectErrResult, expectOkResult)
import HL7Parser exposing (Problem(..))

suite : Test
suite =
    describe "Parse Helpers"
        [ describe "Helpers.parseSizedNumber"
            [ test "parse a number with a fixed size" <|
                \_ ->
                    let
                        source = "12345"
                        parser = parseSizedNumber 5
                    in
                        Expect.equal 12345
                            |> expectOkResult parser source
            , test "parse a larger number with a smaller size" <|
                \_ ->
                    let
                        source = "12345"
                        parser = parseSizedNumber 3
                    in
                        Expect.equal 123
                            |> expectOkResult parser source
            , test "fail to parse a smaller number with a larger size" <|
                \_ ->
                    let
                        source = "123"
                        parser = parseSizedNumber 5
                    in
                        Expect.equal NotANumber
                            |> expectErrResult parser source
            , test "fail to parse non-numeric values" <|
                \_ ->
                    let
                        source = "12a3"
                        parser = parseSizedNumber 4
                    in
                        Expect.equal NotANumber
                            |> expectErrResult parser source
            ]
        , describe "Helpers.parseStringSegment"
            [ test "extract string until a stop character is reached" <|
                \_ ->
                    let
                        source = "the quick, brown fox| jumped over the fence"
                        parser = parseStringSegment
                    in
                        Expect.equal "the quick, brown fox"
                            |> expectOkResult parser source
            ]
        , describe "Helpers.parseStringSegmentOfSize"
            [ test "parse a string segment of fixed size" <|
                        \_ ->
                            let
                                source = "the quick, brown fox| jumped over the fence"
                                parser = parseStringSegmentOfSize 9
                            in
                                Expect.equal "the quick"
                                    |> expectOkResult parser source
            , test "fail to parse a string smaller than size" <|
                \_ ->
                    let
                        source = "the quick"
                        parser = parseStringSegmentOfSize 10
                    in
                        Expect.equal UnexpectedSeparator
                            |> expectErrResult parser source
            , test "fail to parse a string that terminates before size" <|
                \_ ->
                    let
                        source = "the quick brown| fox"
                        parser = parseStringSegmentOfSize 18
                    in
                        Expect.equal UnexpectedSeparator
                            |> expectErrResult parser source
            , test "parse a string that terminates exactly on size" <|
                \_ ->
                    let
                        source = "the quick brown| fox"
                        parser = parseStringSegmentOfSize 15
                    in
                        Expect.equal "the quick brown"
                            |> expectOkResult parser source
            ]
        , describe "Helpers.parseMaybe"
            [ test "turn a successful parser to Maybe result" <|
                \_ ->
                    let
                        parser = succeed "tada!"
                            |> parseMaybe
                    in
                        Expect.equal (Just "tada!")
                            |> expectOkResult parser ""
            , test "turn a un-successful parser to Maybe result" <|
                \_ ->
                    let
                        parser = problem NotANumber
                            |> parseMaybe
                    in
                        Expect.equal Nothing
                            |> expectOkResult parser ""
            ]
        , describe "Helpers.maybeToParser"
            [ test "convert a Just value to a successful parser" <|
                \_ ->
                    let
                        parser = Just "tada!"
                            |> maybeToParser UnexpectedSeparator
                    in
                        Expect.equal "tada!"
                            |> expectOkResult parser "tada!"
            , test "convert a Nothing value to a failed parser" <|
                \_ ->
                    let
                        parser = Nothing
                            |> maybeToParser UnexpectedSeparator
                    in
                        Expect.equal (UnexpectedSeparator)
                            |> expectErrResult parser "tada!"
            ]
        ]
