module Main exposing (main)

import Browser
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html.Attributes
import LangInterpreter as I
import Parser
import LangParser


type alias Model =
    { draft : String, tokens : String, result : Result String I.Val }


main : Program () Model String
main =
    Browser.sandbox
        { init = { draft = "", tokens = "", result = Err "Type the code above!" }
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
    }


preFormatted : E.Attribute msg
preFormatted =
    E.htmlAttribute (Html.Attributes.style "white-space" "pre")


unwrapVal : Result String I.Val -> I.Val
unwrapVal r =
    case r of
        Ok val ->
            val

        Err msg ->
            I.VisVal I.Missing (I.BlockVis (I.CodeblockV [ I.InlineVis <| I.TextV msg ]))


viewVal : I.Val -> E.Element msg
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
            E.text (" " ++ txt ++ " ")

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


viewBlock : I.Block -> E.Element msg
viewBlock vis =
    case vis of
        I.ColumnV vs ->
            E.column
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
                                , E.el [ E.alignTop ] (viewVis v)
                                ]
                        )
                )

        I.ParagraphV is ->
            E.paragraph [ E.width (E.px 800), E.spacing 8 ]
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

        I.AsideV b ->
            E.el
                [ E.width (E.fillPortion 1 |> E.maximum 400)
                , E.alignRight
                , E.alignTop
                , Font.color (E.rgb255 50 50 50)
                , Bg.color (E.rgb255 250 250 250)
                , E.padding 8
                ]
                (viewBlock b)

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
view { draft, result } =
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
        ]
