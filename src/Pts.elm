module Pts exposing (..)

import Parser as P exposing ((|.), (|=), Parser)
import Set exposing (Set)


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


anyChar : Parser Char
anyChar =
    char (always True)


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


yep : a -> a -> a
yep =
    always


nope : a -> a -> a
nope =
    always identity


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


prosePiece : Parser Token
prosePiece =
    P.oneOf
        [ P.succeed identity
            |. char ((==) '$')
            |= identifier
            |> P.map Identifier
        , P.getChompedString (P.chompUntilEndOr "$")
            |> validate "expected something" (String.isEmpty >> not)
            |> P.map Literal
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


pi : Float
pi =
    3.14
