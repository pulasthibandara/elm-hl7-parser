module Main exposing (HD, ID(..), IS(..), MSH, ST(..), TS(..), componentSeparator, escapeCharactor, fieldSeparator, main
    , parseHD, parseID, parseIS, parseMSH, parseST,  parseTS, repeatSeparator, subComponentSeparator, parseDTM, MilliSecondComponent(..)
    , MilliSecondsPrecision(..), SecondsPrecision(..), MinutesPrecision(..), HoursPrecision(..), DaysPrecision(..), MonthsPrecision(..)
    , YearsPrecision(..))

import Helpers exposing (parseMaybe, parseSizedNumber, parseStringSegment, parseStringSegmentOfSize)
import Html exposing (div, text)
import Parser.Advanced exposing ((|.), (|=), Parser, Step(..), Token(..), andThen, backtrackable, chompIf, chompUntil, chompWhile, float, getChompedString, loop, map, oneOf, problem, run, spaces, succeed, symbol, token)
import Result exposing (Result)



-- MAIN


main =
    case run parseMSH "MSH|^~\\&|MILL^asd^asd|asd^asd|MILL^asd^asd|199912210845020123||ADT^A01^ADT_A01|" of
        Result.Err error ->
            div [] [ Debug.toString error |> text ]

        Result.Ok pt ->
            div [] [ Debug.toString pt |> text ]


parseST : Parser x String ST
parseST =
    succeed ST
        |= parseStringSegment


parseTS : Parser x String TS
parseTS =
    succeed TS
        |= parseStringSegment


parseID : Parser x String ID
parseID =
    succeed ID
        |= parseStringSegment

parseNM : Parser x String NM
parseNM =
    succeed NM
        |= float "expecting float" "expecting number"


parseSizedID : Int -> Parser x String ID
parseSizedID size =
    succeed ID
        |= parseStringSegmentOfSize size


parseIS : Parser x String IS
parseIS =
    succeed IS
        |= parseStringSegment


parseHD : Parser x String HD
parseHD =
    oneOf
        [ backtrackable <|
            succeed HD
                |= map Just parseIS
                |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
                |= parseST
                |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
                |= parseID
        , succeed (HD Nothing)
            |= parseST
            |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
            |= parseID
        ]


parseDTM : Parser x String DTM
parseDTM =
    let
        assignToPrecision : (a -> Maybe b -> value) -> Parser x String a -> Parser x String (Maybe b) -> Parser x String value
        assignToPrecision precision parser maybeParser =
            map precision parser
                |> andThen (\p -> map p maybeParser)
    in
    assignToPrecision YearsPrecision (parseSizedNumber 4)
        <| parseMaybe
        <| assignToPrecision MonthsPrecision (parseSizedNumber 2)
        <| parseMaybe
        <| assignToPrecision DaysPrecision (parseSizedNumber 2)
        <| parseMaybe
        <| assignToPrecision HoursPrecision (parseSizedNumber 2)
        <| parseMaybe
        <| assignToPrecision MinutesPrecision (parseSizedNumber 2)
        <| parseMaybe
        <| assignToPrecision SecondsPrecision (parseSizedNumber 2)
        <| parseMaybe
        <| map MilliSecondsPrecision
        <| assignToPrecision MilliSecondComponent (parseSizedNumber 1)
        <| parseMaybe
        <| assignToPrecision MilliSecondComponent (parseSizedNumber 1)
        <| parseMaybe
        <| assignToPrecision MilliSecondComponent (parseSizedNumber 1)
        <| parseMaybe
        <| parseSizedNumber 1


parseMSG : Parser x String MSG
parseMSG =
    succeed MSG
        |= parseSizedID 3
        |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
        |= parseSizedID 3
        |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
        |= parseID

parsePT : Parser x String PT
parsePT =
    succeed PT
        |= parseID
        |. symbol (Token componentSeparator <| "expected symbol '" ++ componentSeparator ++ "'")
        |= parseMaybe parseID

parseCWE : Parser x String CWE
parseCWE =
    succeed CWE
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |= parseDTM
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseDTM
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseDTM

parseVID : Parser x String VID
parseVID =
    succeed VID
        |= parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseCWE
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseCWE

parseEI : Parser x String EI
parseEI =
    succeed EI
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseIS
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseID


parseMSH : Parser x String MSH
parseMSH =
    succeed (MSH (ST fieldSeparator) (ST encodingCharacters))
        |. symbol (Token "MSH" <| "expected symbol '" ++ "MSH" ++ "'")
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |. symbol (Token encodingCharacters <| "expected symbol '" ++ encodingCharacters ++ "'")
        |= parseMaybe parseHD
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseHD
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseHD
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseHD
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseDTM
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMSG
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parsePT
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseVID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseNM
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseST
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseCWE
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseID
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")
        |= parseMaybe parseEI
        |. symbol (Token fieldSeparator <| "expected symbol '" ++ fieldSeparator ++ "'")



-- SEPARATORS


fieldSeparator =
    "|"


componentSeparator =
    "^"


repeatSeparator =
    "~"


escapeCharactor =
    "\\"


subComponentSeparator =
    "&"


encodingCharacters =
    componentSeparator ++ repeatSeparator ++ escapeCharactor ++ subComponentSeparator



-- DATA TYPES
-- PRIMITIVES


type ST
    = ST String -- String data


type TS
    = TS String -- Text data


type ID
    = ID String -- Coded value for HL7 defined tables


type IS
    = IS String -- Coded value for user-defined tables

type NM
    = NM Float  -- Numeric



-- COMPOSITE
-- HD (Hierarchic Designator)


type alias HD =
    { nameSpaceId : Maybe IS, universalId : ST, universalIdType : ID }

-- PT (Processing ID)

type alias PT =
    { processingId: ID, processingMode: Maybe ID }

-- CWE (Coded With Exceptions)
type alias CWE =
    { identifier: Maybe ST
    , text: Maybe ST
    , nameOfCodingSystem: ID
    , alternateIdentifier: Maybe ST
    , alternateText: Maybe ST
    , nameOfAlternateCodingSystem: ID
    , codingSystemVersionID: ST
    , alternateCodingSystemVersionID: Maybe ST
    , originalText: Maybe ST
    , secondAlternateIdentifier: Maybe ST
    , secondAlternateText: Maybe ST
    , nameOfSecondAlternateCodingSystem: ID
    , secondAlternateCodingSystemVersionID: Maybe ST
    , condingSystemOID: ST
    , valueSetOID: Maybe ST
    , valueSetVersionID: DTM
    , alternateCodingSystemOID: ST
    , alternateValueSetOID: Maybe ST
    , alternateValueSetVersionID: DTM
    , secondAlternateCodingSystemOID: ST
    , secondAlternateValueSetOID: Maybe ST
    , secondAlternateValueSetVersionID: DTM
    }

-- VID (Version Identifier)
-- http://www.hl7.eu/refactored/dtVID.html
type alias VID =
    { versionId : ID
    , internationalizationCode : Maybe CWE
    , internationalVersionID : Maybe CWE
    }

-- EI (Entity Identifier)
-- http://www.hl7.eu/refactored/dtEI.html
type alias EI =
    { entityIdentifier : Maybe ST
    , namespaceID : Maybe IS
    , universalID : ST
    , universalIDType : ID
    }

-- DTM (Date/Time)
-- Format: YYYY[MM[DD[HH[MM[SS[.S[S[S[S]]]]]]]]][+/-ZZZZ].


type MilliSecondComponent a
    = MilliSecondComponent Int (Maybe a)


type MilliSecondsPrecision
    = MilliSecondsPrecision (MilliSecondComponent (MilliSecondComponent (MilliSecondComponent Int)))


type SecondsPrecision
    = SecondsPrecision Int (Maybe MilliSecondsPrecision)


type MinutesPrecision
    = MinutesPrecision Int (Maybe SecondsPrecision)


type HoursPrecision
    = HoursPrecision Int (Maybe MinutesPrecision)


type DaysPrecision
    = DaysPrecision Int (Maybe HoursPrecision)


type MonthsPrecision
    = MonthsPrecision Int (Maybe DaysPrecision)


type YearsPrecision
    = YearsPrecision Int (Maybe MonthsPrecision)

type alias DTM =
    YearsPrecision


-- MSG (Message Type)

type alias MSG =
    { messageCode : ID, triggerEvent : ID, messageStructure : ID }



-- SEGMENTS
-- MSH (Message Header)
-- http://www.hl7.eu/refactored/segMSH.html


type alias MSH =
    { fieldSeperator : ST
    , encodingCharacters : ST
    , sendingApplication : Maybe HD
    , sendingFacility : Maybe HD
    , receivingApplication : Maybe HD
    , receivingFacility : Maybe HD
    , dateTimeOfMessage : DTM
    , security : Maybe ST
    , messageType : MSG
    , messageControlID : ST
    , processingID : PT
    , versionID : VID
    , sequenceNumber : Maybe NM
    , continuationPointer : Maybe ST
    , acceptAcknowledgmentType : Maybe ID
    , applicationAcknowledgmentType : Maybe ID
    , countryCode : Maybe ID
    , characterSet : Maybe ID
    , principalLanguageOfMessage : Maybe CWE
    , alternateCharacterSetHandlingScheme : Maybe ID
    , messageProfileIdentifier : Maybe EI
    }
