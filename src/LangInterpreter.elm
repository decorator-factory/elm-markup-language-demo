module LangInterpreter exposing
    ( Block(..)
    , DebugInfo(..)
    , EvalContext
    , EvalError(..)
    , Inline(..)
    , Val(..)
    , Vis(..)
    , defaultCtx
    , evalInContext
    , reprDebug
    )

import Dict exposing (Dict)
import LangParser exposing (Expr(..), Pos, Token(..))


type DebugInfo
    = UserExpr Pos
    | InBuiltin { name : String }
    | CallingFrom { func : DebugInfo, callSite : DebugInfo }
    | Missing


posDebug : Pos -> DebugInfo
posDebug pos =
    UserExpr pos


builtinDebug : String -> DebugInfo
builtinDebug name =
    InBuiltin { name = name }


type EvalContext
    = EvalContext
        { names : Dict String Val
        }


type EvalError
    = UnknownName DebugInfo String
    | NotImplemented DebugInfo String
    | TooManyArgs ( Val, List Val ) DebugInfo
    | NotEnoughArgs Int DebugInfo
    | TypeMismatch DebugInfo String


type Block
    = ColumnV (List Vis)
    | RowV (List Vis)
    | UnorderedListV (List Vis)
    | ParagraphV (List Inline)
    | CodeblockV (List Vis)
    | AsideV Block
    | AnchorV String Block
    | ImageV String


type Inline
    = TextV String
    | InlineCodeV (List Inline)
    | BoldV (List Inline)
    | ItalicV (List Inline)
    | LinkV String (List Inline)
    | FractionV { top : Inline, bottom : Inline, scale : Float }


type Vis
    = BlockVis Block
    | InlineVis Inline
    | NoneV


type Val
    = StrVal DebugInfo String
    | ListVal DebugInfo (List Val)
    | VisVal DebugInfo Vis
    | FnVal DebugInfo (EvalContext -> DebugInfo -> List Val -> Result EvalError ( Val, EvalContext ))


reprDebug : DebugInfo -> String
reprDebug debug =
    case debug of
        UserExpr pos ->
            LangParser.posRepr pos

        InBuiltin { name } ->
            name

        CallingFrom { func, callSite } ->
            "While calling <" ++ reprDebug func ++ ">: " ++ reprDebug callSite

        Missing ->
            "<?>"


debugInfo : Val -> DebugInfo
debugInfo val =
    case val of
        StrVal d _ ->
            d

        ListVal d _ ->
            d

        VisVal d _ ->
            d

        FnVal d _ ->
            d


seqEval : EvalContext -> List Expr -> Result EvalError ( List Val, EvalContext )
seqEval ctx exprs =
    case exprs of
        [] ->
            Ok ( [], ctx )

        e :: es ->
            case evalInContext ctx e of
                Ok ( val, newCtx ) ->
                    seqEval newCtx es |> Result.map (Tuple.mapFirst ((::) val))

                Err err ->
                    Err err


evalInContext : EvalContext -> Expr -> Result EvalError ( Val, EvalContext )
evalInContext (EvalContext ctx) expr =
    case expr of
        StrE pos str ->
            Ok <| ( StrVal (posDebug pos) str, EvalContext ctx )

        NameE pos name ->
            case Dict.get name ctx.names of
                Just val ->
                    Ok ( val, EvalContext ctx )

                Nothing ->
                    Err <| UnknownName (posDebug pos) name

        CallE pos funName argExprs ->
            case Dict.get funName ctx.names of
                Just (StrVal debug s) ->
                    case argExprs of
                        [] ->
                            Ok ( StrVal debug s, EvalContext ctx )

                        _ ->
                            -- TODO: use TooManyArgs?..
                            Err <| TypeMismatch debug "Can only 'call' a string in a no-arguments form, like (dollar)"

                Just (FnVal _ run) ->
                    case seqEval (EvalContext ctx) argExprs of
                        Ok ( vals, newCtx ) ->
                            run newCtx (posDebug pos) vals

                        Err e ->
                            Err e

                Just val ->
                    Err <| TypeMismatch (debugInfo val) "I only know how to call functions"

                Nothing ->
                    Err <| UnknownName (posDebug pos) funName



--- Argument parsing
-- TODO: extract this into a separate module


type alias Args =
    ( List Val, DebugInfo, Int )


type ArgReaderError
    = ArTooManyArgs ( Val, List Val )
    | ArTooFewArgs Int
    | ArWrongArgType Int { expected : String }


type alias ArgReader r =
    Args -> Result ArgReaderError ( r, List Val, Int )


arConst : a -> ArgReader a
arConst a ( args, cs, n ) =
    Ok ( a, args, 1 )


arThen : (a -> ArgReader b) -> ArgReader a -> ArgReader b
arThen fn ra =
    \( args, cs, n ) ->
        ra ( args, cs, n )
            |> Result.andThen (\( a, rest, m ) -> fn a ( rest, cs, m ))
            |> Result.mapError bumpArgPos


arMap : (a -> b) -> ArgReader a -> ArgReader b
arMap fn =
    arThen (fn >> arConst)


arAnd : ArgReader a -> ArgReader (a -> b) -> ArgReader b
arAnd ra rab =
    rab |> arThen (\f -> ra |> arMap f)


bumpArgPos : ArgReaderError -> ArgReaderError
bumpArgPos err =
    case err of
        ArWrongArgType pos details ->
            ArWrongArgType (pos + 1) details

        other ->
            other


type alias ReadOne a =
    Val -> Result (Int -> ArgReaderError) a


arChomp : ReadOne a -> ArgReader a
arChomp read =
    \( args, _, n ) ->
        case args of
            first :: rest ->
                read first
                    |> Result.mapError ((|>) n)
                    |> Result.map (\a -> ( a, rest, n + 1 ))

            [] ->
                Err (ArTooFewArgs n)


arRest : ReadOne a -> ArgReader (List a)
arRest read =
    \( args, cs, n ) ->
        case args of
            item :: items ->
                read item
                    |> Result.mapError ((|>) n)
                    |> Result.andThen
                        (\a ->
                            (arRest read |> arMap ((::) a))
                                ( items, cs, n + 1 )
                        )

            [] ->
                Ok ( [], [], n )


aStr : ReadOne String
aStr arg =
    case arg of
        StrVal _ s ->
            Ok s

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a string" })


aVis : ReadOne Vis
aVis arg =
    case arg of
        StrVal _ s ->
            Ok (InlineVis (TextV s))

        VisVal _ v ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a block or inline element" })


aBlock : ReadOne Block
aBlock arg =
    case arg of
        VisVal _ (BlockVis v) ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a block element" })


anInline : ReadOne Inline
anInline arg =
    case arg of
        StrVal _ s ->
            Ok (TextV s)

        VisVal _ (InlineVis v) ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "an inline element" })


getCallSite : ArgReader DebugInfo
getCallSite ( args, cs, n ) =
    Ok ( cs, args, n )


mapArError : DebugInfo -> ArgReaderError -> EvalError
mapArError cs are =
    case are of
        ArTooFewArgs n ->
            NotEnoughArgs n cs

        ArTooManyArgs ( x, xs ) ->
            TooManyArgs ( x, xs ) cs

        ArWrongArgType argNum { expected } ->
            TypeMismatch cs ("In argument " ++ String.fromInt argNum ++ ": expected " ++ expected)


arConsume : ArgReader a -> ArgReader a
arConsume ra =
    \( args, cs, n ) ->
        case ra ( args, cs, n ) of
            Ok ( a, [], m ) ->
                Ok ( a, [], m )

            Ok ( _, first :: rest, _ ) ->
                Err (ArTooManyArgs ( first, rest ))

            Err other ->
                Err other


buildFn : String -> ArgReader Val -> Val
buildFn name reader =
    FnVal
        (builtinDebug name)
        (\ctx cs args ->
            arConsume reader ( args, CallingFrom { func = builtinDebug name, callSite = cs }, 1 )
                |> Result.mapError (mapArError cs)
                |> Result.map (\( v, _, _ ) -> ( v, ctx ))
        )



-- Some higher-level helpers


manyBlockFun : String -> (List Vis -> Block) -> Val
manyBlockFun name build =
    buildFn name
        (arConst (\cs vs -> VisVal cs (BlockVis <| build vs))
            |> arAnd getCallSite
            |> arAnd (arRest aVis)
        )


manyInlineFun : String -> (List Inline -> Inline) -> Val
manyInlineFun name build =
    buildFn name
        (arConst (\cs vs -> VisVal cs (InlineVis <| build vs))
            |> arAnd getCallSite
            |> arAnd (arRest anInline)
        )


defaultCtx : EvalContext
defaultCtx =
    EvalContext
        { names =
            Dict.fromList
                [ ( "emdash"
                  , StrVal (builtinDebug "emdash") "—"
                  )
                , ( "dollar"
                  , StrVal (builtinDebug "dollar") "$"
                  )
                , ( "article"
                  , manyBlockFun "article" ColumnV
                  )
                , ( "col"
                  , manyBlockFun "col" ColumnV
                  )
                , ( "bullet-list"
                  , manyBlockFun "bullet-list" UnorderedListV
                  )
                , ( "examine"
                  , manyBlockFun "examine" ColumnV
                  )
                , ( "row"
                  , manyBlockFun "row" RowV
                  )
                , ( "para"
                  , buildFn "para"
                        (arConst (\cs vs -> VisVal cs (BlockVis <| ParagraphV vs))
                            |> arAnd getCallSite
                            |> arAnd (arRest anInline)
                        )
                  )
                , ( "code"
                  , manyInlineFun "code" InlineCodeV
                  )
                , ( "code-block"
                  , manyBlockFun "code" CodeblockV
                  )
                , ( "italic"
                  , manyInlineFun "italic" ItalicV
                  )
                , ( "term"
                  , manyInlineFun "term" ItalicV
                  )
                , ( "aside"
                  , buildFn "aside"
                        (arConst (\cs block -> VisVal cs (BlockVis (AsideV block)))
                            |> arAnd getCallSite
                            |> arAnd (arChomp aBlock)
                        )
                  )
                , ( "anchor"
                  , buildFn "anchor"
                        (arConst (\cs anchor block -> VisVal cs (BlockVis (AnchorV anchor block)))
                            |> arAnd getCallSite
                            |> arAnd (arChomp aStr)
                            |> arAnd (arChomp aBlock)
                        )
                  )
                , ( "comment"
                  , FnVal
                        (builtinDebug "comment")
                        (\ctx cs _ -> Ok ( VisVal cs NoneV, ctx ))
                  )
                , ( "link"
                  , buildFn "link"
                        (arConst (\cs anchor vs -> VisVal cs (InlineVis (LinkV anchor vs)))
                            |> arAnd getCallSite
                            |> arAnd (arChomp aStr)
                            |> arAnd (arRest anInline)
                        )
                  )
                , ( "image"
                  , buildFn "image"
                        (arConst (\cs url -> VisVal cs (BlockVis (ImageV url)))
                            |> arAnd getCallSite
                            |> arAnd (arChomp aStr)
                        )
                  )

                -- Math stuff
                , ( "frac"
                  , buildFn "frac"
                        (arConst
                            (\cs top bottom ->
                                VisVal cs (InlineVis (FractionV { top = top, bottom = bottom, scale = 0.85 }))
                            )
                            |> arAnd getCallSite
                            |> arAnd (arChomp anInline)
                            |> arAnd (arChomp anInline)
                        )
                  )
                , ( "big-frac"
                  , buildFn "big-frac"
                        (arConst
                            (\cs top bottom ->
                                VisVal cs (InlineVis (FractionV { top = top, bottom = bottom, scale = 1.0 }))
                            )
                            |> arAnd getCallSite
                            |> arAnd (arChomp anInline)
                            |> arAnd (arChomp anInline)
                        )
                  )
                ]
        }
