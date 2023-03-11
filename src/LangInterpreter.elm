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
    | WrongArgCount DebugInfo
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


type alias Args =
    ( List Val, DebugInfo )


type ArgReaderError
    = ArTooManyArgs
    | ArTooFewArgs
    | ArWrongArgType Int { expected : String }


type alias ArgReader r =
    Args -> Result ArgReaderError r


arConst : a -> ArgReader a
arConst a =
    \_ -> Ok a


arThen : (a -> ArgReader b) -> ArgReader a -> ArgReader b
arThen fn ra =
    \args -> ra args |> Result.andThen (\a -> fn a args)


arMap : (a -> b) -> ArgReader a -> ArgReader b
arMap fn =
    arThen (fn >> arConst)


arDone : a -> ArgReader a
arDone result =
    \( args, _ ) ->
        case args of
            [] ->
                Ok result

            _ ->
                Err ArTooManyArgs


bumpArgPos : ArgReaderError -> ArgReaderError
bumpArgPos err =
    case err of
        ArWrongArgType pos details ->
            ArWrongArgType (pos + 1) details

        other ->
            other


arChomp : ReadOne a -> (a -> ArgReader b) -> ArgReader b
arChomp parse next args =
    case args of
        ( first :: rest, cs ) ->
            parse first
                |> Result.andThen (\a -> next a ( rest, cs ))
                |> Result.mapError bumpArgPos

        ( [], _ ) ->
            Err ArTooFewArgs


arRest : ReadOne a -> ArgReader (List a)
arRest parse args =
    case args of
        ( first :: rest, cs ) ->
            parse first
                |> Result.andThen (\a -> arRest parse ( rest, cs ) |> Result.map ((::) a))
                |> Result.mapError bumpArgPos

        ( [], _ ) ->
            Ok []


type alias ReadOne x =
    Val -> Result ArgReaderError x


aStr : ReadOne String
aStr arg =
    case arg of
        StrVal _ s ->
            Ok s

        _ ->
            Err (ArWrongArgType 0 { expected = "a string" })


aVis : ReadOne Vis
aVis arg =
    case arg of
        StrVal _ s ->
            Ok (InlineVis (TextV s))

        VisVal _ v ->
            Ok v

        _ ->
            Err (ArWrongArgType 1 { expected = "a block or inline element" })


aBlock : ReadOne Block
aBlock arg =
    case arg of
        VisVal _ (BlockVis v) ->
            Ok v

        _ ->
            Err (ArWrongArgType 1 { expected = "a block element" })


anInline : ReadOne Inline
anInline arg =
    case arg of
        StrVal _ s ->
            Ok (TextV s)

        VisVal _ (InlineVis v) ->
            Ok v

        _ ->
            Err (ArWrongArgType 1 { expected = "an inline element" })


getCallSite : ArgReader DebugInfo
getCallSite ( _, cs ) =
    Ok cs


mapArError : DebugInfo -> ArgReaderError -> EvalError
mapArError cs are =
    case are of
        ArTooFewArgs ->
            WrongArgCount cs

        ArTooManyArgs ->
            WrongArgCount cs

        ArWrongArgType argNum { expected } ->
            TypeMismatch cs ("In argument " ++ String.fromInt argNum ++ ": expected " ++ expected)


buildFn : String -> (Args -> Result ArgReaderError Val) -> Val
buildFn name f =
    FnVal
        (builtinDebug name)
        (\ctx cs args ->
            f ( args, cs )
                |> Result.mapError (mapArError cs)
                |> Result.map (\v -> ( v, ctx ))
        )



-- Some higher-level helpers


manyBlockFun : String -> (List Vis -> Block) -> Val
manyBlockFun name impl =
    buildFn name
        (getCallSite
            |> arThen
                (\cs ->
                    arRest aVis
                        |> arMap (\vs -> VisVal cs (BlockVis <| impl vs))
                )
        )


manyInlineFun : String -> (List Inline -> Inline) -> Val
manyInlineFun name impl =
    buildFn name
        (getCallSite
            |> arThen
                (\cs ->
                    arRest anInline
                        |> arMap (\vs -> VisVal cs (InlineVis <| impl vs))
                )
        )


defaultCtx : EvalContext
defaultCtx =
    EvalContext
        { names =
            Dict.fromList
                [ ( "emdash"
                  , StrVal (builtinDebug "emdash") "—"
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
                        (getCallSite
                            |> arThen (\cs -> arRest anInline |> arMap (\vs -> VisVal cs (BlockVis <| ParagraphV vs)))
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
                  , FnVal
                        (builtinDebug "aside")
                        (\ctx cs args ->
                            case args of
                                [ VisVal _ (BlockVis b) ] ->
                                    Ok ( VisVal cs (BlockVis (AsideV b)), ctx )

                                [ _ ] ->
                                    Err (TypeMismatch cs "Expected a block element")

                                _ ->
                                    Err (WrongArgCount cs)
                        )
                  )
                , ( "anchor"
                  , let
                        impl cs anchor block =
                            VisVal cs (BlockVis (AnchorV anchor block))
                    in
                    buildFn "anchor"
                        (getCallSite
                            |> arThen
                                (\cs ->
                                    arChomp aStr <|
                                        \anchor ->
                                            arChomp aBlock <|
                                                \block ->
                                                    arDone (impl cs anchor block)
                                )
                        )
                  )
                , ( "comment"
                  , FnVal
                        (builtinDebug "comment")
                        (\ctx cs _ -> Ok ( VisVal cs NoneV, ctx ))
                  )
                , ( "link"
                  , let
                        impl cs anchor vs =
                            VisVal cs (InlineVis (LinkV anchor vs))
                    in
                    buildFn "link"
                        (getCallSite
                            |> arThen
                                (\cs ->
                                    arChomp aStr <|
                                        \anchor ->
                                            arRest anInline |> arMap (impl cs anchor)
                                )
                        )
                  )
                , ( "image"
                  , FnVal
                        (builtinDebug "image")
                        (\ctx cs args ->
                            case args of
                                [ StrVal _ s ] ->
                                    Ok ( VisVal cs (BlockVis (ImageV s)), ctx )

                                [ _ ] ->
                                    Err (TypeMismatch cs "Expected a string for `image`")

                                [] ->
                                    Err (WrongArgCount cs)

                                _ ->
                                    Err (WrongArgCount cs)
                        )
                  )
                ]
        }
