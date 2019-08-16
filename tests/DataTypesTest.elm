module DataTypesTest exposing (..)

import Expect
import Main exposing (DaysPrecision(..), HoursPrecision(..), MilliSecondComponent(..), MilliSecondsPrecision(..), MinutesPrecision(..), MonthsPrecision(..), SecondsPrecision(..), YearsPrecision(..), parseDTM)
import Test exposing (..)
import TestHelpers exposing (expectOkResult)

dataTypesSuite : Test
dataTypesSuite =
    describe "DataTypes"
        [ describe "DTM (Date/Time)"
            [ test "parse year precision" <|
                \_ ->
                    let
                        source = "2019"
                        parser = parseDTM
                    in
                        Expect.equal (YearsPrecision 2019 Nothing)
                            |> expectOkResult parser source
            , test "parse month precision" <|
                \_ ->
                    let
                        source = "201904"
                        parser = parseDTM
                        expect = YearsPrecision 2019 (Just <| MonthsPrecision 4 Nothing)
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            , test "parse day precision" <|
                \_ ->
                    let
                        source = "20190421"
                        parser = parseDTM
                        expect = YearsPrecision 2019
                            <| Just <| MonthsPrecision 4
                            <| Just <| DaysPrecision 21 Nothing
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            , test "parse hour precision" <|
                \_ ->
                    let
                        source = "2019042110"
                        parser = parseDTM
                        expect = YearsPrecision 2019
                            <| Just <| MonthsPrecision 4
                            <| Just <| DaysPrecision 21
                            <| Just <| HoursPrecision 10 Nothing
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            , test "parse minutes precision" <|
                \_ ->
                    let
                        source = "201904211030"
                        parser = parseDTM
                        expect = YearsPrecision 2019
                            <| Just <| MonthsPrecision 4
                            <| Just <| DaysPrecision 21
                            <| Just <| HoursPrecision 10
                            <| Just <| MinutesPrecision 30 Nothing
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            , test "parse seconds precision" <|
                \_ ->
                    let
                        source = "20190421103005"
                        parser = parseDTM
                        expect = YearsPrecision 2019
                            <| Just <| MonthsPrecision 4
                            <| Just <| DaysPrecision 21
                            <| Just <| HoursPrecision 10
                            <| Just <| MinutesPrecision 30
                            <| Just <| SecondsPrecision 5 Nothing
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            , test "parse milli seconds precision" <|
                \_ ->
                    let
                        source = "201904211030051234"
                        parser = parseDTM
                        expect = YearsPrecision 2019
                            <| Just <| MonthsPrecision 4
                            <| Just <| DaysPrecision 21
                            <| Just <| HoursPrecision 10
                            <| Just <| MinutesPrecision 30
                            <| Just <| SecondsPrecision 5
                            <| Just <| MilliSecondsPrecision
                            <| MilliSecondComponent 1
                            <| Just <| MilliSecondComponent 2
                            <| Just <| MilliSecondComponent 3
                            <| Just <| 4
                    in
                        Expect.equal expect
                            |> expectOkResult parser source
            ]
        ]
