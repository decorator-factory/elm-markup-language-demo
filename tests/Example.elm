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
                        (Pts.consumeExpr [ Pts.Literal Pts.zeroPos "foo" ])
                        (Ok ( Pts.StrE Pts.zeroPos "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.Literal Pts.zeroPos "foo"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "hmm"
                            ]
                        )
                        (Ok
                            ( Pts.StrE Pts.zeroPos "foo"
                            , [ Pts.RightParen Pts.zeroPos
                              , Pts.LeftParen Pts.zeroPos
                              , Pts.Identifier Pts.zeroPos "hmm"
                              ]
                            )
                        )
            ]
        , describe "Name"
            [ test "name" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr [ Pts.Identifier Pts.zeroPos "foo" ])
                        (Ok ( Pts.NameE Pts.zeroPos "foo", [] ))
            , test "string with extra stuff" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.Identifier Pts.zeroPos "foo"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "hmm"
                            ]
                        )
                        (Ok
                            ( Pts.NameE Pts.zeroPos "foo"
                            , [ Pts.RightParen Pts.zeroPos
                              , Pts.LeftParen Pts.zeroPos
                              , Pts.Identifier Pts.zeroPos "hmm"
                              ]
                            )
                        )
            ]
        , describe "Call"
            [ test "empty" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen Pts.zeroPos
                            , Pts.RightParen Pts.zeroPos
                            ]
                        )
                        (Err "Empty lists are not allowed @(line: 1)(while parsing call at (line: 1))")
            , test "no args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "product-name"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.RightParen Pts.zeroPos
                            , Pts.Literal Pts.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE Pts.zeroPos "product-name" []
                            , [ Pts.RightParen Pts.zeroPos
                              , Pts.Literal Pts.zeroPos "heh"
                              ]
                            )
                        )
            , test "1 arg" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "reverse"
                            , Pts.Literal Pts.zeroPos "aibohphobia"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.RightParen Pts.zeroPos
                            , Pts.Literal Pts.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE Pts.zeroPos "reverse" [ Pts.StrE Pts.zeroPos "aibohphobia" ]
                            , [ Pts.RightParen Pts.zeroPos
                              , Pts.Literal Pts.zeroPos "heh"
                              ]
                            )
                        )
            , test "2 args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "concat"
                            , Pts.Literal Pts.zeroPos "foo"
                            , Pts.Literal Pts.zeroPos "bar"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.RightParen Pts.zeroPos
                            , Pts.Literal Pts.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE Pts.zeroPos "concat" [ Pts.StrE Pts.zeroPos "foo", Pts.StrE Pts.zeroPos "bar" ]
                            , [ Pts.RightParen Pts.zeroPos
                              , Pts.Literal Pts.zeroPos "heh"
                              ]
                            )
                        )
            , test "nested args" <|
                \() ->
                    Expect.equal
                        (Pts.consumeExpr
                            [ Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "concat"
                            , Pts.Literal Pts.zeroPos "foo"
                            , Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "concat"
                            , Pts.Literal Pts.zeroPos "bar"
                            , Pts.Literal Pts.zeroPos "fizz"
                            , Pts.Literal Pts.zeroPos "buzz"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.RightParen Pts.zeroPos
                            , Pts.Literal Pts.zeroPos "heh"
                            ]
                        )
                        (Ok
                            ( Pts.CallE Pts.zeroPos
                                "concat"
                                [ Pts.StrE Pts.zeroPos "foo"
                                , Pts.CallE Pts.zeroPos
                                    "concat"
                                    [ Pts.StrE Pts.zeroPos "bar"
                                    , Pts.StrE Pts.zeroPos "fizz"
                                    , Pts.StrE Pts.zeroPos "buzz"
                                    ]
                                ]
                            , [ Pts.Literal Pts.zeroPos "heh"
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
                            parsesLine "\"foo\"" [ Pts.Literal Pts.zeroPos "foo" ]
                    , test "foo bar baz" <|
                        \() ->
                            parsesLine "\"foo bar baz\"" [ Pts.Literal Pts.zeroPos "foo bar baz" ]
                    ]
                , describe "Several double-quoted"
                    [ test "foo, bar, baz" <|
                        \() ->
                            parsesLine "\"foo\" \"bar\" \"baz\"" [ Pts.Literal Pts.zeroPos "foo", Pts.Literal Pts.zeroPos "bar", Pts.Literal Pts.zeroPos "baz" ]
                    ]
                , describe "Escaped"
                    [ test "single" <|
                        \() ->
                            parsesLine "\"foo is a \\\"bar\\\"\"" [ Pts.Literal Pts.zeroPos "foo is a \"bar\"" ]
                    , test "mixed" <|
                        \() ->
                            parsesLine
                                "\"foo is a \\\"bar\\\"\" ( \"\\\\yes\" "
                                [ Pts.Literal Pts.zeroPos "foo is a \"bar\""
                                , Pts.LeftParen Pts.zeroPos
                                , Pts.Literal Pts.zeroPos "\\yes"
                                ]
                    ]
                ]
            ]
        , describe "Prose line"
            [ describe "No interpolation"
                [ test "|foo" <|
                    \() -> parsesLine "| foo" [ Pts.Literal Pts.zeroPos "foo" ]
                , test "|bar" <|
                    \() -> parsesLine "|baroque \\bar \"beats the bartender" [ Pts.Literal Pts.zeroPos "baroque \\bar \"beats the bartender" ]
                , test "empty" <|
                    \() -> parsesLine "|" []
                , test "1 space" <|
                    \() -> parsesLine "| " []
                , test "2 spaces" <|
                    \() -> parsesLine "|  " [ Pts.Literal Pts.zeroPos " " ]
                , test "3 spaces" <|
                    \() -> parsesLine "|   " [ Pts.Literal Pts.zeroPos "  " ]
                , test "indentation is preserved" <|
                    \() -> parsesLine "|     return False" [ Pts.Literal Pts.zeroPos "    return False" ]
                ]
            , describe "Variable substitution"
                [ test "identifier after a $ is parsed as a variable" <|
                    \() ->
                        parsesLine
                            "| Welcome to the $product-name documentation! Are you still using $competitor in $year?"
                            [ Pts.Literal Pts.zeroPos "Welcome to the "
                            , Pts.Identifier Pts.zeroPos "product-name"
                            , Pts.Literal Pts.zeroPos " documentation! Are you still using "
                            , Pts.Identifier Pts.zeroPos "competitor"
                            , Pts.Literal Pts.zeroPos " in "
                            , Pts.Identifier Pts.zeroPos "year"
                            , Pts.Literal Pts.zeroPos "?"
                            ]
                ]
            , describe "Call interpolation"
                [ test "One pair of parentheses" <|
                    \() ->
                        parsesLine
                            "| I am $(foo \"bar\" baz) and also $(hmmm heh)"
                            [ Pts.Literal Pts.zeroPos "I am "
                            , Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "foo"
                            , Pts.Literal Pts.zeroPos "bar"
                            , Pts.Identifier Pts.zeroPos "baz"
                            , Pts.RightParen Pts.zeroPos
                            , Pts.Literal Pts.zeroPos " and also "
                            , Pts.LeftParen Pts.zeroPos
                            , Pts.Identifier Pts.zeroPos "hmmm"
                            , Pts.Identifier Pts.zeroPos "heh"
                            , Pts.RightParen Pts.zeroPos
                            ]
                ]
            ]
            , describe "Does not hang"
                [ test "Case 1" <|
                    \() -> parsesLine
                        "foo bar)"
                        [ Pts.Identifier Pts.zeroPos "foo"
                        , Pts.Identifier Pts.zeroPos "bar"
                        , Pts.RightParen Pts.zeroPos
                        ]
                ]
        ]
