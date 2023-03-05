module Example exposing (..)

import Expect exposing (Expectation)
import Parser
import Pts
import Test exposing (..)


consumeExpr : Test
consumeExpr =
    describe "parseExprFromTokens"
        [ describe "String literal"
            [ test "string" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr [ Pts.Literal "foo" ])
                        (Ok ( Pts.StrE "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.Literal "foo"
                            , Pts.RightParen
                            , Pts.LeftParen
                            , Pts.Identifier "hmm"
                            ]
                        )
                        (Ok
                            ( Pts.StrE "foo"
                            , [ Pts.RightParen
                              , Pts.LeftParen
                              , Pts.Identifier "hmm"
                              ]
                            )
                        )
            ]
        , describe "Name"
            [ test "name" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr [ Pts.Identifier "foo" ])
                        (Ok ( Pts.NameE "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.Identifier "foo"
                            , Pts.RightParen
                            , Pts.LeftParen
                            , Pts.Identifier "hmm"
                            ]
                        )
                        (Ok
                            ( Pts.NameE "foo"
                            , [ Pts.RightParen
                              , Pts.LeftParen
                              , Pts.Identifier "hmm"
                              ]
                            )
                        )
            ]
        , describe "Call"
            [ test "empty" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen
                            , Pts.RightParen
                            ]
                        )
                        (Err "Empty lists are not allowed")
            , test "no args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen
                            , Pts.Identifier "product-name"
                            , Pts.RightParen
                            , Pts.RightParen
                            , Pts.Literal "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE "product-name" []
                            , [ Pts.RightParen
                              , Pts.Literal "heh"
                              ]
                            )
                        )
            , test "1 arg" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen
                            , Pts.Identifier "reverse"
                            , Pts.Literal "aibohphobia"
                            , Pts.RightParen
                            , Pts.RightParen
                            , Pts.Literal "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE "reverse" [ Pts.StrE "aibohphobia" ]
                            , [ Pts.RightParen
                              , Pts.Literal "heh"
                              ]
                            )
                        )
            , test "2 args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen
                            , Pts.Identifier "concat"
                            , Pts.Literal "foo"
                            , Pts.Literal "bar"
                            , Pts.RightParen
                            , Pts.RightParen
                            , Pts.Literal "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE "concat" [ Pts.StrE "foo", Pts.StrE "bar" ]
                            , [ Pts.RightParen
                              , Pts.Literal "heh"
                              ]
                            )
                        )
            , test "nested args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen
                            , Pts.Identifier "concat"
                            , Pts.Literal "foo"
                            , Pts.LeftParen
                            , Pts.Identifier "concat"
                            , Pts.Literal "bar"
                            , Pts.Literal "fizz"
                            , Pts.Literal "buzz"
                            , Pts.RightParen
                            , Pts.RightParen
                            , Pts.Literal "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE "concat"
                                [ Pts.StrE "foo"
                                , Pts.CallE "concat"
                                    [ Pts.StrE "bar"
                                    , Pts.StrE "fizz"
                                    , Pts.StrE "buzz"
                                    ]
                                ]
                            , [ Pts.Literal "heh"
                              ]
                            )
                        )
            ]
        ]


tokenizeLine : Test
tokenizeLine =
    let
        parsesLine : String -> List Pts.Token -> Expectation
        parsesLine line expectedTokens =
            case Parser.run Pts.sourceLineP line of
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
                    \() -> parsesLine "|  " [ Pts.Literal " " ]
                , test "3 spaces" <|
                    \() -> parsesLine "|   " [ Pts.Literal "  " ]
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
