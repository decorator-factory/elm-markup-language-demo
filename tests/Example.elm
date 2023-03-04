module Example exposing (..)

import Expect exposing (Expectation)
import Pts
import Test exposing (..)


tokenizeLine : Test
tokenizeLine =
    let
        parsesLine : String -> List Pts.Token -> Expectation
        parsesLine line expectedTokens =
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
                    \() -> parsesLine "" []
                , test "whitespace" <|
                    \() -> parsesLine "               " []
                ]
            , describe "String literals"
                [ describe "Simple double-quoted"
                    [ test "foo" <|
                        \() ->
                            parsesLine "\"foo\"" [ Pts.Literal "foo" ]
                    , test "foo bar baz" <|
                        \() ->
                            parsesLine "\"foo bar baz\"" [ Pts.Literal "foo bar baz" ]
                    ]
                , describe "Several double-quoted"
                    [ test "foo, bar, baz" <|
                        \() ->
                            parsesLine "\"foo\" \"bar\" \"baz\"" [ Pts.Literal "foo", Pts.Literal "bar", Pts.Literal "baz" ]
                    ]
                , describe "Escaped"
                    [ test "single" <|
                        \() ->
                            parsesLine "\"foo is a \\\"bar\\\"\"" [ Pts.Literal "foo is a \"bar\"" ]
                    , test "mixed" <|
                        \() ->
                            parsesLine
                                "\"foo is a \\\"bar\\\"\" ( \"\\\\yes\" "
                                [ Pts.Literal "foo is a \"bar\"", Pts.LeftParen, Pts.Literal "\\yes" ]
                    ]
                ]
            ]
        , describe "Prose line"
            [ describe "No interpolation"
                [ test "|foo" <|
                    \() -> parsesLine "| foo" [ Pts.Literal "foo" ]
                , test "|bar" <|
                    \() -> parsesLine "|baroque \\bar \"beats the bartender" [ Pts.Literal "baroque \\bar \"beats the bartender" ]
                , test "empty" <|
                    \() -> parsesLine "|" []
                , test "1 space" <|
                    \() -> parsesLine "| " []
                , test "2 spaces" <|
                    \() -> parsesLine "|  " []
                , test "3 spaces" <|
                    \() -> parsesLine "|   " []
                , test "4 spaces" <|
                    \() -> parsesLine "|    " []
                , test "indentation is preserved" <|
                    \() -> parsesLine "|     return False" [ Pts.Literal "    return False" ]
                ]
            , describe "Variable substitution"
                [ test "identifier after a $ is parsed as a variable" <|
                    \() ->
                        parsesLine
                            "| Welcome to the $product-name documentation! Are you still using $competitor in $year?"
                            [ Pts.Literal "Welcome to the "
                            , Pts.Identifier "product-name"
                            , Pts.Literal " documentation! Are you still using "
                            , Pts.Identifier "competitor"
                            , Pts.Literal " in "
                            , Pts.Identifier "year"
                            , Pts.Literal "?"
                            ]
                ]
            , describe "Call interpolation"
                [ test "One pair of parentheses" <|
                    \() ->
                        parsesLine
                            "| I am $(foo \"bar\" baz) and also $(hmmm heh)"
                            [ Pts.Literal "I am "
                            , Pts.LeftParen
                            , Pts.Identifier "foo"
                            , Pts.Literal "bar"
                            , Pts.Identifier "baz"
                            , Pts.RightParen
                            , Pts.Literal " and also "
                            , Pts.LeftParen
                            , Pts.Identifier "hmmm"
                            , Pts.Identifier "heh"
                            , Pts.RightParen
                            ]
                ]
            ]
        ]
