module HL7Parser exposing (..)

import Parser.Advanced as A

type Parser a = Parser (Ctx -> A.Parser Never Problem a)

type Ctx = Ctx

infix left 5 (|=) = keeper
infix left 6 (|.) = ignorer


{-| Just like [`Parser.succeed`](Parser#succeed)
-}
succeed : a -> Parser a
succeed a =
  Parser <| \_ ->
    A.succeed a

{-| Just like [`Parser.problem`](Parser#problem) except you provide a custom
type for your problem.
-}
problem : Problem -> Parser a
problem x =
    Parser <| \_ ->
      A.problem x

{-| Just like [`Parser.map`](Parser#map)
-}
map : (a -> b) -> Parser a -> Parser b
map func (Parser parse) =
    Parser <| \ctx ->
        A.map func <| parse ctx

map2 : (a -> b -> value) -> Parser a -> Parser b -> Parser value
map2 func (Parser parseA) (Parser parseB) =
    Parser <| \ctx ->
        let
            ap = parseA ctx
            bp = parseB ctx
        in
            A.andThen (\a -> A.map (\b -> func a b) bp) ap

{-| Just like the [`(|=)`](Parser#|=) from the `Parser` module.
-}
keeper : Parser (a -> b) -> Parser a -> Parser b
keeper parseFunc parseArg =
    let
        func : (a -> b) -> a -> b
        func fn ap = fn ap
    in
  map2 func parseFunc parseArg

{-| Just like the [`(|.)`](Parser#|.) from the `Parser` module.
-}
ignorer : Parser keep -> Parser ignore -> Parser keep
ignorer keepParser ignoreParser =
  map2 always keepParser ignoreParser

-- AND THEN


{-| Just like [`Parser.andThen`](Parser#andThen)
-}
andThen : (a -> Parser b) -> Parser a -> Parser b
andThen callback (Parser parseA) =
  Parser <| \ctx ->
    A.andThen (\a -> (callback a) ctx) (parseA ctx)


type Problem
    = NotANumber
    | NotAFloat
    | NotANumberOfSize Int
    | ExpectedSeparator String
    | ExpectedSymbol String
    | UnexpectedSeparator
