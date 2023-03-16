module Main exposing (main)

import Browser
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import LangInterpreter as I
import LangParser
import Parser


type alias Val =
    I.Val LangParser.TextPos


type alias Model =
    { draft : String, tokens : String, result : Result String Val, parsed : Result String (LangParser.Expr ()) }


main : Program () Model String
main =
    Browser.sandbox
        { init = { draft = "", tokens = "", result = Err "Type the code above!", parsed = Err "Nothing yet" }
        , view = view >> E.layout [ E.width E.fill ]
        , update = update
        }


update : String -> Model -> Model
update msg model =
    let
        expr =
            Parser.run LangParser.program msg

        document =
            expr |> Result.map (I.evalInContext I.defaultCtx)
    in
    { model
        | draft = msg
        , result =
            case document of
                Ok (Ok ( doc, _ )) ->
                    Ok doc

                Err e ->
                    Err (Debug.toString e)

                Ok (Err e) ->
                    Err (Debug.toString e)
        , parsed =
            expr
                |> Result.map (LangParser.mapLoc (always ()))
                |> Result.mapError Debug.toString
    }


preFormatted : E.Attribute msg
preFormatted =
    E.htmlAttribute (Html.Attributes.style "white-space" "pre")


unwrapVal : Result String Val -> Val
unwrapVal r =
    case r of
        Ok val ->
            val

        Err msg ->
            I.VisVal I.Missing (I.BlockVis (I.CodeblockV [ I.InlineVis <| I.TextV msg ]))


viewVal : Val -> E.Element msg
viewVal val =
    case val of
        I.VisVal _ vis ->
            viewVis vis

        other ->
            E.text (Debug.toString other)


viewInline : I.Inline -> E.Element msg
viewInline vis =
    case vis of
        I.TextV txt ->
            E.text (" " ++ txt)

        I.InlineCodeV vs ->
            E.paragraph
                [ preFormatted
                , Font.family [ Font.monospace ]
                ]
                (List.map viewInline vs)

        I.BoldV vs ->
            E.paragraph [ Font.bold ] (List.map viewInline vs)

        I.ItalicV vs ->
            E.paragraph [ Font.italic ] (List.map viewInline vs)

        I.LinkV tag vs ->
            E.link [ Font.color (E.rgb255 37 92 186), Font.underline ]
                { url = "#elm_vis__" ++ tag
                , label = E.paragraph [ Font.bold ] (List.map viewInline vs)
                }

        I.FractionV { top, bottom, scale } ->
            E.column
                [ E.spacing 4
                , verticalAlignMiddle
                , E.scale scale
                ]
                [ E.el
                    [ E.alignBottom
                    , E.centerX
                    , E.paddingXY 8 0
                    ]
                    (viewInline top)
                , E.el
                    [ E.width E.fill
                    , E.height E.shrink
                    , Border.color (E.rgb255 30 30 30)
                    , Border.width 1
                    , E.paddingXY 4 0
                    ]
                    E.none
                , E.el
                    [ E.alignTop
                    , E.centerX
                    , E.paddingXY 8 0
                    ]
                    (viewInline bottom)
                ]

        I.SuperscriptV { subject, detail } ->
            case subject of
                I.SubscriptV inner ->
                    viewSubSup (viewInline inner.subject) (viewInline inner.detail) (viewInline detail)

                other ->
                    viewSubSup (viewInline other) E.none (viewInline detail)

        I.SubscriptV { subject, detail } ->
            case subject of
                I.SuperscriptV inner ->
                    viewSubSup (viewInline inner.subject) (viewInline detail) (viewInline inner.detail)

                other ->
                    viewSubSup (viewInline other) (viewInline detail) E.none

        I.InlineSeq vs ->
            E.paragraph [] <| List.map viewInline vs


viewSubSup : E.Element msg -> E.Element msg -> E.Element msg -> E.Element msg
viewSubSup subject sub sup =
    -- This is a bit of a mess! Let's think how to solve it later...
    E.paragraph []
        [ E.row []
            [ E.el [ E.alignRight ] subject
            , E.paragraph []
                [ E.column [ E.scale 0.75 ]
                    [ E.el
                        [ E.height (E.fillPortion 1)
                        , E.alignLeft
                        , E.moveUp 12
                        , E.moveLeft 2
                        ]
                        sup
                    , E.el [ E.height (E.fillPortion 1) ] E.none
                    , E.el
                        [ E.height (E.fillPortion 1)
                        , E.alignLeft
                        , E.moveUp 12
                        , E.moveLeft 2
                        ]
                        sub
                    ]
                ]
            ]
        ]


viewBlock : I.Block -> E.Element msg
viewBlock vis =
    case vis of
        I.ColumnV vs ->
            E.textColumn
                [ E.spacing 32
                , E.width (E.fillPortion 5 |> E.maximum 1000)
                ]
                (List.map viewVis vs)

        I.RowV vs ->
            E.row
                [ E.spacing 16
                , E.width (E.fill |> E.maximum 1000)
                ]
                (List.map viewVis vs)

        I.UnorderedListV vs ->
            E.column [ E.spacing 16 ]
                (vs
                    |> List.map
                        (\v ->
                            E.row [ E.spacing 8 ]
                                [ E.el [ E.alignTop, E.paddingXY 4 0 ] (E.text "•")
                                , E.el [ E.alignTop, E.width E.fill ] (viewVis v)
                                ]
                        )
                )

        I.ParagraphV is ->
            E.paragraph [ E.width (E.fillPortion 4 |> E.maximum 800), E.spacing 8 ]
                (List.map viewInline is)

        I.CodeblockV vs ->
            E.column
                [ E.padding 16
                , E.spacingXY 0 16
                , preFormatted
                , Font.family [ Font.monospace ]
                , Bg.color (E.rgb255 250 250 250)
                , Border.widthEach { left = 2, right = 0, top = 0, bottom = 0 }
                , Border.color (E.rgb255 37 92 186)
                ]
                (List.map viewVis vs)

        I.AsideV v ->
            E.el
                [ E.width (E.fillPortion 1 |> E.maximum 500)
                , E.alignRight
                , E.alignTop
                , Font.color (E.rgb255 50 50 50)
                , Bg.color (E.rgb255 240 240 250)
                , E.padding 16
                ]
                (viewVis v)

        I.AnchorV tag b ->
            E.el [ E.htmlAttribute <| Html.Attributes.id ("elm_vis__" ++ tag) ] (viewBlock b)

        I.ImageV url ->
            E.image [ E.width (E.fill |> E.maximum 800) ]
                { src = url
                , description = url
                }


viewVis : I.Vis -> E.Element msg
viewVis vis =
    case vis of
        I.BlockVis b ->
            viewBlock b

        I.InlineVis i ->
            viewInline i

        I.NoneV ->
            E.none


view : Model -> E.Element String
view { draft, result, parsed } =
    E.column [ E.padding 32, E.spacing 16, E.width E.fill ]
        [ Input.multiline
            [ E.width E.fill
            , Font.family [ Font.monospace ]
            ]
            { onChange = identity
            , text = draft
            , placeholder = Nothing
            , label = Input.labelAbove [] <| E.text "Code"
            , spellcheck = False
            }
        , result |> unwrapVal |> viewVal
        , case parsed of
            Ok p ->
                E.paragraph [E.width (E.px 600)] [E.text (Debug.toString p)]

            Err _ ->
                E.none
        ]



--- Some utilities
--
-- a bit of a hack, but I haven't found a way to vertically align an inline-block
-- inside of a paragraph line:


verticalAlignMiddle : E.Attribute msg
verticalAlignMiddle =
    E.htmlAttribute (Html.Attributes.style "vertical-align" "middle")
