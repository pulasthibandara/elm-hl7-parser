module Helpers exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), andThen, backtrackable, chompIf, chompUntil, chompWhile, float, getChompedString, loop, map, oneOf, problem, spaces, succeed, symbol, token)
import Result exposing (Result)

-- Parser helpers

parseStringSegment : Parser String
parseStringSegment =
    chompWhile (\c -> c /= '|' && c /= '^') |> getChompedString


parseStringSegmentOfSize : Int -> Parser String
parseStringSegmentOfSize size =
    let
        chompOne : Int -> Parser (Step Int ())
        chompOne sizeState =
            if sizeState < 1 then
                succeed (Done ())
            else
                succeed (Loop (sizeState - 1))
                    |. chompIf (\c -> c /= '|' && c /= '^')
    in
    loop size chompOne |> getChompedString

parseMaybe : Parser a -> Parser (Maybe a)
parseMaybe parser =
    oneOf
        [ backtrackable <| map Just parser
        , succeed Nothing
        ]


maybeToParser : String -> Maybe a -> Parser a
maybeToParser err maybe =
    Maybe.map succeed maybe
        |> Maybe.withDefault (problem err)


parseSizedNumber : Int -> Parser Int
parseSizedNumber size =
    let
        getDigits : Int -> Parser () -> Parser ()
        getDigits s parser =
            if s < 1 then
                parser

            else
                getDigits (s - 1) (parser |. chompIf Char.isDigit)
    in
    getDigits size (succeed ())
        |> getChompedString
        |> map String.toInt
        |> andThen (maybeToParser ("Not a number of size: " ++ String.fromInt size))
