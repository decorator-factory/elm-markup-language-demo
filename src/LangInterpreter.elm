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
import LangParser exposing (Expr(..), TextPos, Token(..))


type DebugInfo loc
    = InExpr loc
    | InUserFunction { origin : DebugInfo loc }
    | InBuiltin { name : String }
    | CallingFrom { func : DebugInfo loc, callSite : DebugInfo loc }
    | Missing


posDebug : loc -> DebugInfo loc
posDebug pos =
    InExpr pos


builtinDebug : String -> DebugInfo loc
builtinDebug name =
    InBuiltin { name = name }


type EvalContext loc
    = EvalContext
        { names : Dict String (Val loc)
        }


type EvalError loc
    = UnknownName (DebugInfo loc) String
    | MacroError (DebugInfo loc) String
    | NotImplemented (DebugInfo loc) String
    | TooManyArgs ( Val loc, List (Val loc) ) (DebugInfo loc)
    | NotEnoughArgs Int (DebugInfo loc)
    | TypeMismatch (DebugInfo loc) String


type Block
    = ColumnV (List Vis)
    | RowV (List Vis)
    | UnorderedListV (List Vis)
    | ParagraphV (List Inline)
    | CodeblockV (List Vis)
    | AsideV Vis
    | AnchorV String Block
    | ImageV String


type Inline
    = TextV String
    | InlineCodeV (List Inline)
    | BoldV (List Inline)
    | ItalicV (List Inline)
    | LinkV String (List Inline)
    | FractionV { top : Inline, bottom : Inline, scale : Float }
    | SuperscriptV { subject : Inline, detail : Inline }
    | SubscriptV { subject : Inline, detail : Inline }
    | InlineSeq (List Inline)


type Vis
    = BlockVis Block
    | InlineVis Inline
    | NoneV


type alias FnVal loc =
    EvalContext loc -> DebugInfo loc -> List (Val loc) -> Result (EvalError loc) ( Val loc, EvalContext loc )


type alias MacroFnVal loc =
    EvalContext loc -> DebugInfo loc -> List (Expr loc) -> Result (EvalError loc) ( Val loc, EvalContext loc )


type Val loc
    = StrVal (DebugInfo loc) String
    | ListVal (DebugInfo loc) (List (Val loc))
    | VisVal (DebugInfo loc) Vis
    | FnVal (DebugInfo loc) (FnVal loc)
    | MacroFnVal (DebugInfo loc) (MacroFnVal loc)


reprDebug : DebugInfo loc -> String
reprDebug debug =
    case debug of
        InExpr pos ->
            LangParser.posRepr pos

        InBuiltin { name } ->
            name

        CallingFrom { func, callSite } ->
            "While calling <" ++ reprDebug func ++ ">: " ++ reprDebug callSite

        InUserFunction { origin } ->
            "While calling function defined at " ++ reprDebug origin

        Missing ->
            "<?>"


debugInfo : Val loc -> DebugInfo loc
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

        MacroFnVal d _ ->
            d


type alias EvalFn loc =
    EvalContext loc -> Expr loc -> Result (EvalError loc) ( Val loc, EvalContext loc )


seqEval : EvalFn loc -> EvalContext loc -> List (Expr loc) -> Result (EvalError loc) ( List (Val loc), EvalContext loc )
seqEval eval ctx exprs =
    case exprs of
        [] ->
            Ok ( [], ctx )

        e :: es ->
            case eval ctx e of
                Ok ( val, newCtx ) ->
                    seqEval eval newCtx es |> Result.map (Tuple.mapFirst ((::) val))

                Err err ->
                    Err err


callFnByName :
    EvalFn loc
    -> EvalContext loc
    -> TextPos
    -> String
    -> List (Expr loc)
    -> Result (EvalError loc) ( Val loc, EvalContext loc )
callFnByName eval (EvalContext ctx) pos funName argExprs =
    case Dict.get funName ctx.names of
        Just fun ->
            callFnByValue eval (EvalContext ctx) pos fun argExprs

        Nothing ->
            Err <| UnknownName (posDebug pos) funName


callFnByValue :
    EvalFn loc
    -> EvalContext loc
    -> TextPos
    -> Val loc
    -> List (Expr loc)
    -> Result (EvalError loc) ( Val loc, EvalContext loc )
callFnByValue eval (EvalContext ctx) pos fun argExprs =
    case fun of
        StrVal debug s ->
            case argExprs of
                [] ->
                    Ok ( StrVal debug s, EvalContext ctx )

                _ ->
                    -- TODO: use TooManyArgs?..
                    Err <| TypeMismatch debug "Can only 'call' a string in a no-arguments form, like (dollar)"

        FnVal _ run ->
            case seqEval eval (EvalContext ctx) argExprs of
                Ok ( vals, newCtx ) ->
                    run newCtx (posDebug pos) vals

                Err e ->
                    Err e

        MacroFnVal _ run ->
            run (EvalContext ctx) (posDebug pos) argExprs

        val ->
            Err <| TypeMismatch (debugInfo val) "I only know how to call functions"


evalInContext : EvalFn loc
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
            callFnByName evalInContext (EvalContext ctx) pos funName argExprs



--- Argument parsing
-- TODO: extract this into a separate module


type alias Args loc =
    ( List (Val loc), DebugInfo loc, Int )


type ArgReaderError loc
    = ArTooManyArgs ( Val loc, List (Val loc) )
    | ArTooFewArgs Int
    | ArWrongArgType Int { expected : String }


type alias ArgReader r loc =
    Args loc -> Result (ArgReaderError loc) ( r, List (Val loc), Int )


arConst : a -> ArgReader a loc
arConst a ( args, cs, n ) =
    Ok ( a, args, 1 )


arThen : (a -> ArgReader b loc) -> ArgReader a loc -> ArgReader b loc
arThen fn ra =
    \( args, cs, n ) ->
        ra ( args, cs, n )
            |> Result.andThen (\( a, rest, m ) -> fn a ( rest, cs, m ))
            |> Result.mapError bumpArgPos


arMap : (a -> b) -> ArgReader a loc -> ArgReader b loc
arMap fn =
    arThen (fn >> arConst)


arAnd : ArgReader a loc -> ArgReader (a -> b) loc -> ArgReader b loc
arAnd ra rab =
    rab |> arThen (\f -> ra |> arMap f)


bumpArgPos : ArgReaderError loc -> ArgReaderError loc
bumpArgPos err =
    case err of
        ArWrongArgType pos details ->
            ArWrongArgType (pos + 1) details

        other ->
            other


type alias ReadOne a loc =
    Val loc -> Result (Int -> ArgReaderError loc) a


arChomp : ReadOne a loc -> ArgReader a loc
arChomp read =
    \( args, _, n ) ->
        case args of
            first :: rest ->
                read first
                    |> Result.mapError ((|>) n)
                    |> Result.map (\a -> ( a, rest, n + 1 ))

            [] ->
                Err (ArTooFewArgs n)


arRest : ReadOne a loc -> ArgReader (List a) loc
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


aStr : ReadOne String loc
aStr arg =
    case arg of
        StrVal _ s ->
            Ok s

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a string" })


aVis : ReadOne Vis loc
aVis arg =
    case arg of
        StrVal _ s ->
            Ok (InlineVis (TextV s))

        VisVal _ v ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a block or inline element" })


aBlock : ReadOne Block loc
aBlock arg =
    case arg of
        VisVal _ (BlockVis v) ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "a block element" })


anInline : ReadOne Inline loc
anInline arg =
    case arg of
        StrVal _ s ->
            Ok (TextV s)

        VisVal _ (InlineVis v) ->
            Ok v

        _ ->
            Err (\n -> ArWrongArgType n { expected = "an inline element" })


getCallSite : ArgReader (DebugInfo loc) loc
getCallSite ( args, cs, n ) =
    Ok ( cs, args, n )


mapArError : DebugInfo loc -> ArgReaderError loc -> EvalError loc
mapArError cs are =
    case are of
        ArTooFewArgs n ->
            NotEnoughArgs n cs

        ArTooManyArgs ( x, xs ) ->
            TooManyArgs ( x, xs ) cs

        ArWrongArgType argNum { expected } ->
            TypeMismatch cs ("In argument " ++ String.fromInt argNum ++ ": expected " ++ expected)


arConsume : ArgReader a loc -> ArgReader a loc
arConsume ra =
    \( args, cs, n ) ->
        case ra ( args, cs, n ) of
            Ok ( a, [], m ) ->
                Ok ( a, [], m )

            Ok ( _, first :: rest, _ ) ->
                Err (ArTooManyArgs ( first, rest ))

            Err other ->
                Err other


buildFn : String -> ArgReader (Val loc) loc -> Val loc
buildFn name reader =
    FnVal
        (builtinDebug name)
        (\ctx cs args ->
            arConsume reader ( args, CallingFrom { func = builtinDebug name, callSite = cs }, 1 )
                |> Result.mapError (mapArError cs)
                |> Result.map (\( v, _, _ ) -> ( v, ctx ))
        )



-- Some higher-level helpers


manyBlockFun : String -> (List Vis -> Block) -> Val loc
manyBlockFun name build =
    buildFn name
        (arConst (\cs vs -> VisVal cs (BlockVis <| build vs))
            |> arAnd getCallSite
            |> arAnd (arRest aVis)
        )


manyInlineFun : String -> (List Inline -> Inline) -> Val loc
manyInlineFun name build =
    buildFn name
        (arConst (\cs vs -> VisVal cs (InlineVis <| build vs))
            |> arAnd getCallSite
            |> arAnd (arRest anInline)
        )


defaultCtx : EvalContext loc
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
                , ( "bold"
                  , manyInlineFun "bold" BoldV
                  )
                , ( "term"
                  , manyInlineFun "term" ItalicV
                  )
                , ( "aside"
                  , buildFn "aside"
                        (arConst (\cs block -> VisVal cs (BlockVis (AsideV block)))
                            |> arAnd getCallSite
                            |> arAnd (arChomp aVis)
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
                , ( "cc"
                  , buildFn "cc"
                        (arConst
                            (\cs vs -> VisVal cs (InlineVis (InlineSeq vs)))
                            |> arAnd getCallSite
                            |> arAnd (arRest anInline)
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
                , ( "sup"
                  , buildFn "sup"
                        (arConst
                            (\cs subject detail ->
                                VisVal cs (InlineVis (SuperscriptV { subject = subject, detail = detail }))
                            )
                            |> arAnd getCallSite
                            |> arAnd (arChomp anInline)
                            |> arAnd (arChomp anInline)
                        )
                  )
                , ( "sub"
                  , buildFn "sub"
                        (arConst
                            (\cs subject detail ->
                                VisVal cs (InlineVis (SubscriptV { subject = subject, detail = detail }))
                            )
                            |> arAnd getCallSite
                            |> arAnd (arChomp anInline)
                            |> arAnd (arChomp anInline)
                        )
                  )
                , ( "gr"
                  , buildFn "gr"
                        (arConst
                            (\cs vs -> VisVal cs (InlineVis <| InlineSeq <| List.concat [ [ TextV "(" ], vs, [ TextV ")" ] ]))
                            |> arAnd getCallSite
                            |> arAnd (arRest anInline)
                        )
                  )

                -- Definitions
                , ( "def"
                  , MacroFnVal
                        (builtinDebug "def")
                        (\ctx callSite argExprs ->
                            case argExprs of
                                [ NameE _ name, expr ] ->
                                    evalInContext ctx expr
                                        |> Result.map
                                            (\( val, EvalContext newCtx ) ->
                                                ( VisVal callSite NoneV
                                                , EvalContext <| { newCtx | names = newCtx.names |> Dict.insert name val }
                                                )
                                            )

                                [ _, _ ] ->
                                    Err (TypeMismatch callSite "First argument to def should be a name, like: (def my-name \"alice\")")

                                _ ->
                                    Err (MacroError callSite "Expected exactly 2 arguments: name and value")
                        )
                  )
                , ( "defun"
                  , MacroFnVal
                        (builtinDebug "defun")
                        (\ctx callSite defunArgExprs ->
                            case defunArgExprs of
                                [ CallE callPos funName argExprs, bodyExpr ] ->
                                    case argExprs of
                                        (NameE argNamesPos firstArg) :: restArgs ->
                                            evalInContext ctx
                                                (CallE callPos
                                                    "def"
                                                    [ NameE callPos funName
                                                    , CallE callPos
                                                        "fun"
                                                        [ CallE argNamesPos firstArg restArgs
                                                        , bodyExpr
                                                        ]
                                                    ]
                                                )

                                        _ :: _ ->
                                            Err (MacroError (InExpr callPos) "Expected an argument name, like %foo")

                                        [] ->
                                            Err (MacroError callSite "If you don't want to take any arguments, use a 'def'")

                                [ _, _ ] ->
                                    Err (TypeMismatch callSite "Expected a form like (add %x %y) as the first argument to defun")

                                _ ->
                                    Err (TypeMismatch callSite "Expected exactly two arguments to defun")
                        )
                  )
                , ( "fun"
                  , let
                        parseArg : Expr loc -> Result (EvalError loc) String
                        parseArg expr =
                            case expr of
                                NameE pos name ->
                                    if name |> String.startsWith "%" then
                                        Ok (name |> String.slice 1 -1)

                                    else
                                        Err (MacroError (InExpr pos) "Expected argument name to start with %")

                                other ->
                                    Err (MacroError (InExpr <| LangParser.exprPos other) "Expected an argument name like %foo")

                        parseArgs : List (Expr loc) -> Result (EvalError loc) (List String)
                        parseArgs exprs =
                            case exprs of
                                e :: es ->
                                    Result.map2
                                        (::)
                                        (parseArg e)
                                        (parseArgs es)

                                [] ->
                                    Ok []
                    in
                    MacroFnVal
                        (builtinDebug "fun")
                        (\defCtx defCallSite argExprs ->
                            case argExprs of
                                [ CallE pos firstName restNamesExprs, body ] ->
                                    let
                                        newFn argNames =
                                            FnVal
                                                (InUserFunction { origin = defCallSite })
                                                (\callCtx callSite argVals ->
                                                    case zipExact argNames argVals of
                                                        JustRight kvs ->
                                                            evalWithReplacements (Dict.fromList kvs) callCtx body

                                                        LeftOverflow ( _, names ) ->
                                                            Err (NotEnoughArgs (List.length names) callSite)

                                                        RightOverflow ( val, vals ) ->
                                                            Err (TooManyArgs ( val, vals ) callSite)
                                                )
                                    in
                                    parseArgs (NameE pos firstName :: restNamesExprs)
                                        |> Result.map (\argNames -> ( newFn argNames, defCtx ))

                                [ _, _ ] ->
                                    Err (TypeMismatch defCallSite "Expected a form like (%x %y) as the first argument to fun")

                                _ ->
                                    Err (TypeMismatch defCallSite "Expected exactly two arguments to fun")
                        )
                  )
                ]
        }


evalWithReplacements : Dict String (Val loc) -> EvalFn loc
evalWithReplacements vars ctx expr =
    case expr of
        NameE _ name ->
            if name |> String.startsWith "%" then
                case Dict.get (name |> String.slice 1 -1) vars of
                    Just val ->
                        Ok ( val, ctx )

                    Nothing ->
                        evalInContext ctx expr

            else
                evalInContext ctx expr

        StrE _ _ ->
            evalInContext ctx expr

        CallE pos funName argExprs ->
            if funName |> String.startsWith "%" then
                case Dict.get (funName |> String.slice 1 -1) vars of
                    Just fun ->
                        callFnByValue (evalWithReplacements vars) ctx pos fun argExprs

                    Nothing ->
                        evalInContext ctx expr

            else
                callFnByName (evalWithReplacements vars) ctx pos funName argExprs


type ZipExact a b
    = JustRight (List ( a, b ))
    | LeftOverflow ( a, List a )
    | RightOverflow ( b, List b )


zipExact : List a -> List b -> ZipExact a b
zipExact xl yl =
    case ( xl, yl ) of
        ( [], [] ) ->
            JustRight []

        ( x :: xr, [] ) ->
            LeftOverflow ( x, xr )

        ( [], y :: yr ) ->
            RightOverflow ( y, yr )

        ( x :: xr, y :: yr ) ->
            case
                zipExact xr yr
            of
                JustRight prev ->
                    JustRight (( x, y ) :: prev)

                other ->
                    other
