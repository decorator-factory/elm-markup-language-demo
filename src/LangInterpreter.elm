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


resultSeqHelp : List (() -> Result e a) -> List a -> Result e (List a)
resultSeqHelp rs acc =
    case rs of
        [] ->
            Ok acc

        x :: xs ->
            case x () of
                Ok a ->
                    resultSeqHelp xs (a :: acc)

                Err e ->
                    Err e


resultSeq : List (() -> Result e a) -> Result e (List a)
resultSeq rs =
    resultSeqHelp rs [] |> Result.map List.reverse


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


expectVis : Val -> Result EvalError Vis
expectVis val =
    case val of
        VisVal _ v ->
            Ok v

        StrVal _ s ->
            Ok (InlineVis (TextV s))

        other ->
            Err (TypeMismatch (debugInfo other) "Expected a Vis")


expectInline : Val -> Result EvalError Inline
expectInline vis =
    case vis of
        VisVal _ (InlineVis v) ->
            Ok v

        StrVal _ s ->
            Ok (TextV s)

        other ->
            Err (TypeMismatch (debugInfo other) "Expected an inline element")


expectVisuals : List Val -> Result EvalError (List Vis)
expectVisuals exprs =
    resultSeq (exprs |> List.map (\val () -> expectVis val))


expectInlines : List Val -> Result EvalError (List Inline)
expectInlines exprs =
    resultSeq (exprs |> List.map (\val () -> expectInline val))



-- This is horrible and duplicated. I am sorry


manyBlockFun : String -> (List Vis -> Block) -> Val
manyBlockFun name impl =
    FnVal
        (builtinDebug name)
        (\ctx callSite vals ->
            expectVisuals vals
                |> Result.map (\vs -> ( VisVal callSite (BlockVis <| impl vs), ctx ))
        )


manyInlineFun : String -> (List Inline -> Inline) -> Val
manyInlineFun name impl =
    FnVal
        (builtinDebug name)
        (\ctx callSite vals ->
            expectInlines vals
                |> Result.map (\vs -> ( VisVal callSite (InlineVis <| impl vs), ctx ))
        )


manyInlineFunTagged : String -> (String -> List Inline -> Inline) -> Val
manyInlineFunTagged name impl =
    FnVal
        (builtinDebug name)
        (\ctx callSite vals ->
            case vals of
                [] ->
                    Err (WrongArgCount callSite)

                x :: xs ->
                    case x of
                        StrVal _ s ->
                            expectInlines xs
                                |> Result.map (\vs -> ( VisVal callSite <| InlineVis <| impl s vs, ctx ))

                        _ ->
                            Err (TypeMismatch callSite "Expected a string as the first argument")
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
                  , FnVal
                        (builtinDebug "para")
                        (\ctx callSite vals ->
                            expectInlines vals
                                |> Result.map (\vs -> ( VisVal callSite (BlockVis <| ParagraphV vs), ctx ))
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
                        (\ctx callSite args ->
                            case args of
                                [ VisVal _ (BlockVis b) ] ->
                                    Ok ( VisVal callSite (BlockVis (AsideV b)), ctx )

                                [ _ ] ->
                                    Err (TypeMismatch callSite "Expected a block element")

                                _ ->
                                    Err (WrongArgCount callSite)
                        )
                  )
                , ( "anchor"
                  , FnVal
                        (builtinDebug "anchor")
                        (\ctx callSite args ->
                            case args of
                                [ StrVal _ tag, VisVal _ (BlockVis b) ] ->
                                    Ok ( VisVal callSite (BlockVis (AnchorV tag b)), ctx )

                                [ _, _ ] ->
                                    Err (TypeMismatch callSite "Expected a string and a block element")

                                _ ->
                                    Err (WrongArgCount callSite)
                        )
                  )
                , ( "comment"
                  , FnVal
                        (builtinDebug "comment")
                        (\ctx callSite _ -> Ok ( VisVal callSite NoneV, ctx ))
                  )
                , ( "link"
                  , manyInlineFunTagged "link" LinkV
                  )
                , ( "image"
                  , FnVal
                        (builtinDebug "image")
                        (\ctx callSite args ->
                            case args of
                                [ StrVal _ s ] ->
                                    Ok ( VisVal callSite (BlockVis (ImageV s)), ctx )

                                [ _ ] ->
                                    Err (TypeMismatch callSite "Expected a string for `image`")

                                [] ->
                                    Err (WrongArgCount callSite)

                                _ ->
                                    Err (WrongArgCount callSite)
                        )
                  )
                ]
        }
