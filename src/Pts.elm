module Pts exposing (Expr(..), Token(..), sourceLine)

import Parser as P exposing ((|.), (|=), Parser)
import Set


type Expr
    = StrE String
    | NameE String
    | CallE String (List Expr)


type Token
    = LeftParen
    | RightParen
    | Identifier String
    | Literal String


identifier : P.Parser String
identifier =
    P.variable
        { start = Char.isLower
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


oneToken : P.Parser Token
oneToken =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ P.symbol "(" |> P.map (always LeftParen)
            , P.symbol ")" |> P.map (always RightParen)
            , identifier |> P.map Identifier
            , literal |> P.map Literal
            ]


codeLine : Parser (List Token)
codeLine =
    P.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = P.spaces
        , item = oneToken
        , trailing = P.Optional
        }


validate : String -> (a -> Bool) -> Parser a -> Parser a
validate desc pred =
    P.andThen
        (\a ->
            if pred a then
                P.succeed a

            else
                P.problem desc
        )


prosePiece : Parser (List Token)
prosePiece =
    P.oneOf
        [ P.symbol "$$"
            |> P.map (always [ Literal "$" ])
        , P.succeed identity
            |. P.symbol "$"
            |= P.oneOf
                [ identifier
                    |> P.map (Identifier >> List.singleton)
                , P.symbol "("
                    |> P.andThen (always balancedParens)
                    |> P.map ((::) LeftParen)
                ]
        , P.getChompedString (P.chompUntilEndOr "$")
            |> validate "expected something" (String.isEmpty >> not)
            |> P.map (Literal >> List.singleton)
        ]


proseLine : Parser (List Token)
proseLine =
    P.sequence
        { start = ""
        , separator = ""
        , end = ""
        , spaces = P.succeed ()
        , item = prosePiece
        , trailing = P.Optional
        }
        |> P.map List.concat


type Nat
    = Zero
    | Suc Nat


type alias BpStep =
    ( Nat, List Token )


balancedParensHelp : BpStep -> Parser (P.Step BpStep (List Token))
balancedParensHelp ( depth, acc ) =
    P.succeed identity
        |. P.spaces
        |= P.oneOf
            [ identifier
                |> P.map Identifier
                |> P.map (\tok -> P.Loop ( depth, tok :: acc ))
            , literal
                |> P.map Literal
                |> P.map (\tok -> P.Loop ( depth, tok :: acc ))
            , P.symbol "("
                |> P.map (always <| P.Loop ( Suc depth, LeftParen :: acc ))
            , P.symbol ")"
                |> (case depth of
                        Zero ->
                            P.map (always <| P.Done (RightParen :: acc))

                        Suc n ->
                            P.map (always <| P.Loop ( n, RightParen :: acc ))
                   )
            ]


balancedParens : Parser (List Token)
balancedParens =
    P.succeed identity
        |. P.spaces
        |= P.loop ( Zero, [] ) balancedParensHelp
        |> P.map List.reverse


sourceLine : String -> Result (List P.DeadEnd) (List Token)
sourceLine line =
    let
        trimmed =
            String.trim line
    in
    if String.startsWith "| " trimmed then
        P.run proseLine (String.dropLeft 2 trimmed)

    else if String.startsWith "|" trimmed then
        P.run proseLine (String.dropLeft 1 trimmed)

    else
        P.run codeLine trimmed
