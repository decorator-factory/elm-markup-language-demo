module LangParser exposing
    ( Expr(..)
    , TextPos
    , Token(..)
    , consumeExpr
    , oneExpr
    , posRepr
    , program
    , sourceLineP
    , tokenize
    , zeroPos, exprPos
    )

import Json.Encode as Je
import Parser as P exposing ((|.), (|=), Parser)
import Set


type TextPos
    = Pos { line : Int }


type Expr loc
    = StrE loc String
    | NameE loc String
    | CallE loc String (List (Expr loc))


exprPos : Expr loc -> loc
exprPos expr =
    case expr of
        StrE pos _ -> pos
        NameE pos _ -> pos
        CallE pos _ _ -> pos


type Token
    = LeftParen TextPos
    | RightParen TextPos
    | Identifier TextPos String
    | Literal TextPos String


tokenize : P.Parser (List Token)
tokenize =
    P.loop []
        (\prev ->
            P.oneOf
                [ P.end |> P.map (always (P.Done prev))
                , sourceLineP |> P.map ((++) prev) |> P.map P.Loop
                ]
        )


program : P.Parser (Expr TextPos)
program =
    P.succeed identity
        |= oneExpr
        |. P.end


oneExpr : P.Parser (Expr TextPos)
oneExpr =
    tokenize
        |> P.andThen
            (\tokens ->
                case consumeExpr tokens of
                    Ok ( expr, [] ) ->
                        P.succeed expr

                    Ok ( _, _ ) ->
                        P.problem "Got some extra tokens while parsing expression"

                    Err err ->
                        P.problem err
            )


zeroPos : TextPos
zeroPos =
    Pos { line = 1 }


posRepr : TextPos -> String
posRepr (Pos { line }) =
    "(line: " ++ String.fromInt line ++ ")"


tokenRepr : Token -> String
tokenRepr tok =
    case tok of
        LeftParen pos ->
            "LeftParen @" ++ posRepr pos

        RightParen pos ->
            "RightParen @" ++ posRepr pos

        Identifier pos s ->
            "(Identifier " ++ Je.encode 0 (Je.string s) ++ " @" ++ posRepr pos ++ ")"

        Literal pos s ->
            "(Literal " ++ Je.encode 0 (Je.string s) ++ " @" ++ posRepr pos ++ ")"


consumeExpr : List Token -> Result String ( Expr TextPos, List Token )
consumeExpr tokens =
    case tokens of
        [] ->
            Err "Unexpected end of input"

        (RightParen pos) :: _ ->
            Err <| "Unmatched ) @" ++ posRepr pos

        (Literal pos s) :: rest ->
            Ok ( StrE pos s, rest )

        (Identifier pos s) :: rest ->
            Ok ( NameE pos s, rest )

        (LeftParen pos) :: rest ->
            parseCall rest |> Result.mapError (\s -> s ++ "(while parsing call at " ++ posRepr pos ++ ")")


parseCall : List Token -> Result String ( Expr TextPos, List Token )
parseCall tokens =
    case tokens of
        [] ->
            Err "unexpected EOI while parsing a call"

        (Identifier pos fname) :: rest ->
            parseCallArgs rest
                |> Result.map
                    (\( args, rem ) ->
                        ( CallE pos fname args, rem )
                    )

        (RightParen pos) :: _ ->
            Err <| "Empty lists are not allowed @" ++ posRepr pos

        badToken :: _ ->
            Err <| "Expected a function name, got: " ++ tokenRepr badToken


parseCallArgs : List Token -> Result String ( List (Expr TextPos), List Token )
parseCallArgs tokens =
    case tokens of
        [] ->
            Err "unexpected EOI while parsing call arguments"

        (RightParen _) :: rest ->
            Ok ( [], rest )

        rest ->
            consumeExpr rest
                |> Result.andThen
                    (\( expr, rem1 ) ->
                        parseCallArgs rem1
                            |> Result.map
                                (\( args, rem2 ) ->
                                    ( expr :: args, rem2 )
                                )
                    )


identifier : P.Parser String
identifier =
    P.variable
        { start = \c -> Char.isLower c || c == '%'
        , inner = \c -> Char.isLower c || Char.isDigit c || c == '-'
        , reserved = Set.empty
        }


char : (Char -> Bool) -> P.Parser Char
char pred =
    P.getChompedString (P.chompIf pred)
        |> P.map (String.uncons >> Maybe.map Tuple.first)
        |> P.andThen (Maybe.map P.succeed >> Maybe.withDefault (P.problem "impossible"))


literalChar : P.Parser Char
literalChar =
    P.oneOf
        [ P.symbol "\\\\" |> P.map (always '\\')
        , P.symbol "\\\"" |> P.map (always '"')
        , char (\c -> c /= '\\' && c /= '"')
        ]


literal : P.Parser String
literal =
    P.sequence
        { start = "\""
        , separator = ""
        , end = "\""
        , spaces = P.succeed ()
        , item = literalChar
        , trailing = P.Optional
        }
        |> P.map (List.foldr String.cons "")


currentPos : P.Parser TextPos
currentPos =
    P.getRow |> P.map (\row -> Pos { line = row })


oneToken : P.Parser Token
oneToken =
    P.succeed identity
        |. P.chompWhile ((==) ' ')
        |= P.oneOf
            [ P.symbol "(" |> P.andThen (\_ -> currentPos |> P.map LeftParen)
            , P.symbol ")" |> P.andThen (\_ -> currentPos |> P.map RightParen)
            , identifier |> P.andThen (\s -> currentPos |> P.map (\p -> Identifier p s))
            , literal |> P.andThen (\s -> currentPos |> P.map (\p -> Literal p s))
            ]


codeLine : Parser (List Token)
codeLine =
    P.loop
        []
        (\ts ->
            P.succeed identity
                |. P.chompWhile ((==) ' ')
                |= P.oneOf
                    [ P.end |> P.map (always (P.Done ts))
                    , P.symbol "\n" |> P.map (always (P.Done ts))
                    , oneToken |> P.map (\t -> P.Loop (t :: ts))
                    ]
        )
        |> P.map List.reverse


proseLine : Parser (List Token)
proseLine =
    P.loop []
        (\ts ->
            currentPos
                |> P.andThen
                    (\pos ->
                        P.oneOf
                            [ P.end |> P.map (always (P.Done ts))
                            , P.symbol "\n" |> P.map (always (P.Done ts))
                            , P.succeed identity
                                |. P.symbol "$"
                                |= P.oneOf
                                    [ identifier
                                        |> P.map (Identifier pos >> List.singleton)
                                    , P.symbol "("
                                        |> P.andThen (always balancedParens)
                                        |> P.map ((::) (LeftParen pos))
                                    ]
                                |> P.map ((++) ts)
                                |> P.map P.Loop
                            , P.getChompedString (P.chompWhile (\c -> c /= '$' && c /= '\n'))
                                |> P.andThen
                                    (\s ->
                                        if s == "" then
                                            P.problem "bruh"

                                        else
                                            P.succeed [ Literal pos s ]
                                    )
                                |> P.map ((++) ts)
                                |> P.map P.Loop
                            ]
                    )
        )


type Nat
    = Zero
    | Suc Nat


type alias BpStep =
    ( Nat, List Token )


balancedParensHelp : BpStep -> Parser (P.Step BpStep (List Token))
balancedParensHelp ( depth, acc ) =
    currentPos
        |> P.andThen
            (\pos ->
                P.succeed identity
                    |. P.chompWhile ((==) ' ')
                    |= P.oneOf
                        [ identifier
                            |> P.map (Identifier pos)
                            |> P.map (\tok -> P.Loop ( depth, tok :: acc ))
                        , literal
                            |> P.map (Literal pos)
                            |> P.map (\tok -> P.Loop ( depth, tok :: acc ))
                        , P.symbol "("
                            |> P.map (always <| P.Loop ( Suc depth, LeftParen pos :: acc ))
                        , P.symbol ")"
                            |> P.map
                                (always <|
                                    case depth of
                                        Zero ->
                                            P.Done <| RightParen pos :: acc

                                        Suc n ->
                                            P.Loop ( n, RightParen pos :: acc )
                                )
                        ]
            )


balancedParens : Parser (List Token)
balancedParens =
    P.succeed identity
        |. P.chompWhile ((==) ' ')
        |= P.loop ( Zero, [] ) balancedParensHelp
        |> P.map List.reverse


sourceLineP : P.Parser (List Token)
sourceLineP =
    P.succeed identity
        |. P.chompWhile ((==) ' ')
        |= P.oneOf
            [ P.succeed identity
                |. P.symbol "| "
                |= proseLine
            , P.succeed identity
                |. P.symbol "|"
                |= proseLine
            , codeLine
            ]
