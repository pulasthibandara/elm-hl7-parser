module Helpers exposing (..)

import HL7Parser exposing (Problem(..), Ctx)
import Parser.Advanced exposing ((|.), (|=), Parser, Step(..), andThen, backtrackable, chompIf, chompUntil, chompWhile, float, getChompedString, loop, map, oneOf, problem, spaces, succeed, symbol, token)
import Result exposing (Result)

-- Parser helpers

parseStringSegment : Parser c Problem String
parseStringSegment =
    chompWhile (\c -> c /= '|' && c /= '^') |> getChompedString


parseStringSegmentOfSize : Int -> Parser c Problem String
parseStringSegmentOfSize size =
    let
        chompOne : Int -> Parser c Problem (Step Int ())
        chompOne sizeState =
            if sizeState < 1 then
                succeed (Done ())
            else
                succeed (Loop (sizeState - 1))
                    |. chompIf (\c -> c /= '|' && c /= '^') UnexpectedSeparator
    in
    loop size chompOne |> getChompedString

parseMaybe : Parser c Problem a -> Parser c Problem (Maybe a)
parseMaybe parser =
    oneOf
        [ backtrackable <| map Just parser
        , succeed Nothing
        ]


maybeToParser : Problem -> Maybe a -> Parser c Problem a
maybeToParser err maybe =
    Maybe.map succeed maybe
        |> Maybe.withDefault (problem err)


parseSizedNumber : Int -> Parser c Problem Int
parseSizedNumber size =
    let
        getDigits : Int -> Parser c Problem () -> Parser c Problem ()
        getDigits s parser =
            if s < 1 then
                parser

            else
                getDigits (s - 1) (parser |. chompIf Char.isDigit NotANumber)
    in
    getDigits size (succeed ())
        |> getChompedString
        |> map String.toInt
        |> andThen (maybeToParser (NotANumberOfSize size))
