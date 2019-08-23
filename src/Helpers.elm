module Helpers exposing (..)

import Parser.Advanced exposing ((|.), (|=), Parser, Step(..), andThen, backtrackable, chompIf, chompUntil, chompWhile, float, getChompedString, loop, map, oneOf, problem, spaces, succeed, symbol, token)
import Result exposing (Result)

-- Parser helpers

parseStringSegment : Parser c String String
parseStringSegment =
    chompWhile (\c -> c /= '|' && c /= '^') |> getChompedString


parseStringSegmentOfSize : Int -> Parser c String String
parseStringSegmentOfSize size =
    let
        chompOne : Int -> Parser c String (Step Int ())
        chompOne sizeState =
            if sizeState < 1 then
                succeed (Done ())
            else
                succeed (Loop (sizeState - 1))
                    |. chompIf (\c -> c /= '|' && c /= '^') "Unexpected segment separator"
    in
    loop size chompOne |> getChompedString

parseMaybe : Parser c String a -> Parser c String (Maybe a)
parseMaybe parser =
    oneOf
        [ backtrackable <| map Just parser
        , succeed Nothing
        ]


maybeToParser : String -> Maybe a -> Parser c String a
maybeToParser err maybe =
    Maybe.map succeed maybe
        |> Maybe.withDefault (problem err)


parseSizedNumber : Int -> Parser c String Int
parseSizedNumber size =
    let
        getDigits : Int -> Parser c String () -> Parser c String ()
        getDigits s parser =
            if s < 1 then
                parser

            else
                getDigits (s - 1) (parser |. chompIf Char.isDigit "Not a digit")
    in
    getDigits size (succeed ())
        |> getChompedString
        |> map String.toInt
        |> andThen (maybeToParser ("Not a number of size: " ++ String.fromInt size))
