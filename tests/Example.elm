module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Parser as P
import Pts
import Test exposing (..)


tokenizeLine : Test
tokenizeLine =
    let
        parses : String -> List Pts.Token -> Expectation
        parses line expectedTokens =
            case Pts.sourceLine line of
                Ok actualTokens ->
                    actualTokens
                        |> Expect.equalLists expectedTokens

                Err e ->
                    Expect.fail <| Debug.toString e
    in
    describe "tokenizeLine"
        [ describe "Code line"
            [ describe "No tokens"
                [ test "empty" <|
                    \() -> parses "" []
                , test "whitespace" <|
                    \() -> parses "               " []
                ]
            , describe "String literals"
                [ describe "Simple double-quoted"
                    [ test "foo" <|
                        \() ->
                            parses "\"foo\"" [ Pts.Literal "foo" ]
                    , test "foo bar baz" <|
                        \() ->
                            parses "\"foo bar baz\"" [ Pts.Literal "foo bar baz" ]
                    ]
                , describe "Several double-quoted"
                    [ test "foo, bar, baz" <|
                        \() ->
                            parses "\"foo\" \"bar\" \"baz\"" [ Pts.Literal "foo", Pts.Literal "bar", Pts.Literal "baz" ]
                    ]
                , describe "Escaped"
                    [ test "single" <|
                        \() ->
                            parses "\"foo is a \\\"bar\\\"\"" [ Pts.Literal "foo is a \"bar\"" ]
                    , test "mixed" <|
                        \() ->
                            parses
                                "\"foo is a \\\"bar\\\"\" ( \"\\\\yes\" "
                                [ Pts.Literal "foo is a \"bar\"", Pts.LeftParen, Pts.Literal "\\yes" ]
                    ]
                ]
            ]
        , describe "Prose line"
            [ describe "No interpolation"
                [ test "|foo" <|
                    \() -> parses "| foo" [ Pts.Literal "foo" ]
                ]
            ]
        ]
