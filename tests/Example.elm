module Example exposing (..)

import Expect exposing (Expectation)
import Parser
import LangParser as LP
import Test exposing (..)


consumeExpr : Test
consumeExpr =
    describe "parseExprFromTokens"
        [ describe "String literal"
            [ test "string" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr [ LP.Literal LP.zeroPos "foo" ])
                        (Ok ( LP.StrE LP.zeroPos "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.Literal LP.zeroPos "foo"
                            , LP.RightParen LP.zeroPos
                            , LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "hmm"
                            ]
                        )
                        (Ok
                            ( LP.StrE LP.zeroPos "foo"
                            , [ LP.RightParen LP.zeroPos
                              , LP.LeftParen LP.zeroPos
                              , LP.Identifier LP.zeroPos "hmm"
                              ]
                            )
                        )
            ]
        , describe "Name"
            [ test "name" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr [ LP.Identifier LP.zeroPos "foo" ])
                        (Ok ( LP.NameE LP.zeroPos "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.Identifier LP.zeroPos "foo"
                            , LP.RightParen LP.zeroPos
                            , LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "hmm"
                            ]
                        )
                        (Ok
                            ( LP.NameE LP.zeroPos "foo"
                            , [ LP.RightParen LP.zeroPos
                              , LP.LeftParen LP.zeroPos
                              , LP.Identifier LP.zeroPos "hmm"
                              ]
                            )
                        )
            ]
        , describe "Call"
            [ test "empty" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.LeftParen LP.zeroPos
                            , LP.RightParen LP.zeroPos
                            ]
                        )
                        (Err "Empty lists are not allowed @(line: 1)(while parsing call at (line: 1))")
            , test "no args" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "product-name"
                            , LP.RightParen LP.zeroPos
                            , LP.RightParen LP.zeroPos
                            , LP.Literal LP.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( LP.CallE LP.zeroPos "product-name" []
                            , [ LP.RightParen LP.zeroPos
                              , LP.Literal LP.zeroPos "heh"
                              ]
                            )
                        )
            , test "1 arg" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "reverse"
                            , LP.Literal LP.zeroPos "aibohphobia"
                            , LP.RightParen LP.zeroPos
                            , LP.RightParen LP.zeroPos
                            , LP.Literal LP.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( LP.CallE LP.zeroPos "reverse" [ LP.StrE LP.zeroPos "aibohphobia" ]
                            , [ LP.RightParen LP.zeroPos
                              , LP.Literal LP.zeroPos "heh"
                              ]
                            )
                        )
            , test "2 args" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "concat"
                            , LP.Literal LP.zeroPos "foo"
                            , LP.Literal LP.zeroPos "bar"
                            , LP.RightParen LP.zeroPos
                            , LP.RightParen LP.zeroPos
                            , LP.Literal LP.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( LP.CallE LP.zeroPos "concat" [ LP.StrE LP.zeroPos "foo", LP.StrE LP.zeroPos "bar" ]
                            , [ LP.RightParen LP.zeroPos
                              , LP.Literal LP.zeroPos "heh"
                              ]
                            )
                        )
            , test "nested args" <|
                \() ->
                    Expect.equal
                        (LP.consumeExpr
                            [ LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "concat"
                            , LP.Literal LP.zeroPos "foo"
                            , LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "concat"
                            , LP.Literal LP.zeroPos "bar"
                            , LP.Literal LP.zeroPos "fizz"
                            , LP.Literal LP.zeroPos "buzz"
                            , LP.RightParen LP.zeroPos
                            , LP.RightParen LP.zeroPos
                            , LP.Literal LP.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( LP.CallE LP.zeroPos
                                "concat"
                                [ LP.StrE LP.zeroPos "foo"
                                , LP.CallE LP.zeroPos
                                    "concat"
                                    [ LP.StrE LP.zeroPos "bar"
                                    , LP.StrE LP.zeroPos "fizz"
                                    , LP.StrE LP.zeroPos "buzz"
                                    ]
                                ]
                            , [ LP.Literal LP.zeroPos "heh"
                              ]
                            )
                        )
            ]
        ]


tokenizeLine : Test
tokenizeLine =
    let
        parsesLine : String -> List LP.Token -> Expectation
        parsesLine line expectedTokens =
            case Parser.run LP.sourceLineP line of
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
                            parsesLine "\"foo\"" [ LP.Literal LP.zeroPos "foo" ]
                    , test "foo bar baz" <|
                        \() ->
                            parsesLine "\"foo bar baz\"" [ LP.Literal LP.zeroPos "foo bar baz" ]
                    ]
                , describe "Several double-quoted"
                    [ test "foo, bar, baz" <|
                        \() ->
                            parsesLine "\"foo\" \"bar\" \"baz\"" [ LP.Literal LP.zeroPos "foo", LP.Literal LP.zeroPos "bar", LP.Literal LP.zeroPos "baz" ]
                    ]
                , describe "Escaped"
                    [ test "single" <|
                        \() ->
                            parsesLine "\"foo is a \\\"bar\\\"\"" [ LP.Literal LP.zeroPos "foo is a \"bar\"" ]
                    , test "mixed" <|
                        \() ->
                            parsesLine
                                "\"foo is a \\\"bar\\\"\" ( \"\\\\yes\" "
                                [ LP.Literal LP.zeroPos "foo is a \"bar\""
                                , LP.LeftParen LP.zeroPos
                                , LP.Literal LP.zeroPos "\\yes"
                                ]
                    ]
                ]
            ]
        , describe "Prose line"
            [ describe "No interpolation"
                [ test "|foo" <|
                    \() -> parsesLine "| foo" [ LP.Literal LP.zeroPos "foo" ]
                , test "|bar" <|
                    \() -> parsesLine "|baroque \\bar \"beats the bartender" [ LP.Literal LP.zeroPos "baroque \\bar \"beats the bartender" ]
                , test "empty" <|
                    \() -> parsesLine "|" []
                , test "1 space" <|
                    \() -> parsesLine "| " []
                , test "2 spaces" <|
                    \() -> parsesLine "|  " [ LP.Literal LP.zeroPos " " ]
                , test "3 spaces" <|
                    \() -> parsesLine "|   " [ LP.Literal LP.zeroPos "  " ]
                , test "indentation is preserved" <|
                    \() -> parsesLine "|     return False" [ LP.Literal LP.zeroPos "    return False" ]
                ]
            , describe "Variable substitution"
                [ test "identifier after a $ is parsed as a variable" <|
                    \() ->
                        parsesLine
                            "| Welcome to the $product-name documentation! Are you still using $competitor in $year?"
                            [ LP.Literal LP.zeroPos "Welcome to the "
                            , LP.Identifier LP.zeroPos "product-name"
                            , LP.Literal LP.zeroPos " documentation! Are you still using "
                            , LP.Identifier LP.zeroPos "competitor"
                            , LP.Literal LP.zeroPos " in "
                            , LP.Identifier LP.zeroPos "year"
                            , LP.Literal LP.zeroPos "?"
                            ]
                ]
            , describe "Call interpolation"
                [ test "One pair of parentheses" <|
                    \() ->
                        parsesLine
                            "| I am $(foo \"bar\" baz) and also $(hmmm heh)"
                            [ LP.Literal LP.zeroPos "I am "
                            , LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "foo"
                            , LP.Literal LP.zeroPos "bar"
                            , LP.Identifier LP.zeroPos "baz"
                            , LP.RightParen LP.zeroPos
                            , LP.Literal LP.zeroPos " and also "
                            , LP.LeftParen LP.zeroPos
                            , LP.Identifier LP.zeroPos "hmmm"
                            , LP.Identifier LP.zeroPos "heh"
                            , LP.RightParen LP.zeroPos
                            ]
                ]
            ]
            , describe "Does not hang"
                [ test "Case 1" <|
                    \() -> parsesLine
                        "foo bar)"
                        [ LP.Identifier LP.zeroPos "foo"
                        , LP.Identifier LP.zeroPos "bar"
                        , LP.RightParen LP.zeroPos
                        ]
                ]
        ]
