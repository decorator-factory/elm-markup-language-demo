module Editor exposing (main)

import Array exposing (Array)
import Array.Extra as Ae
import Browser
import Editor.Ui as Ui
import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html
import Html.Attributes
import Json.Encode as Je
import LangParser as Lp exposing (Expr(..))


type alias Expr =
    Lp.Expr ()


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = viewHtml
        , update = update
        }


type alias Pos =
    ( Int, List Int )


type alias Items =
    Array Expr


listGet : Int -> List a -> Maybe a
listGet idx =
    List.drop idx >> List.head


drill : List Int -> Expr -> Maybe Expr
drill xs expr =
    case ( xs, expr ) of
        ( [], any ) ->
            Just any

        ( i :: is, Lp.CallE _ _ argExprs ) ->
            listGet i argExprs |> Maybe.andThen (drill is)

        _ ->
            Nothing


type alias Model =
    { items : Items
    , selection : Maybe Pos
    }


sample : List Expr
sample =
    [ CallE ()
        "comment"
        [ StrE () <|
            "Adapted from: https://htdp.org\n"
                ++ "CC BY-NC-ND (linked interpretation):\n"
                ++ "> [y]ou are free to <..> copy and redistribute the material in any medium or format\n"
                ++ "> Merely changing the format never creates a derivative`\n"
        ]
    , CallE ()
        "col"
        [ CallE ()
            "row"
            [ CallE ()
                "para"
                [ StrE () <|
                    "When you were a small child, your parents taught you to count "
                        ++ "and perform simple calculations with your fingers: \"1 + 1 is 2\"; "
                        ++ "\"1 + 2 is 3\"; and so on. Then they would ask \"what's 3 + 2?\" and "
                        ++ "you would count off the fingers of one hand. They programmed, and "
                        ++ "you computed. And in some way, that's really all there is to "
                        ++ "programming and computing. "
                ]
            , CallE ()
                "aside"
                [ CallE ()
                    "para"
                    [ StrE () "Consider a quick look at "
                    , CallE () "link" [ StrE () "on-teaching", StrE () "On Teaching" ]
                    , StrE () "."
                    ]
                ]
            ]
        , CallE ()
            "col"
            [ CallE ()
                "para"
                [ StrE () "Now it is time to switch roles. Start DrRacket. Doing so brings up the window of"
                , CallE () "link" [ StrE () "meet-drracket", StrE () "Meet DrRacket" ]
                , StrE () <|
                    ". Select “Choose language” from the “Language” menu, which opens a dialog\n"
                        ++ "listing “Teaching Languages” for “How to Design Programs.” Choose “Beginning Student”\n"
                        ++ "(the Beginning Student Language, or BSL) and click \n"
                , CallE () "italic" [ StrE () "OK" ]
                , StrE () <|
                    " to set up DrRacket.\n"
                        ++ "With this task completed, you can program, and the DrRacket software becomes the child.\n"
                        ++ "Start with the simplest of all calculations. You type\n"
                ]
            , CallE ()
                "code-block"
                [ StrE () "(+ 1 1)"
                ]
            , CallE ()
                "para"
                [ StrE () "into the top part of DrRacket, click "
                , CallE () "italic" [ StrE () "RUN" ]
                , StrE () ", and a "
                , CallE () "code" [ StrE () "2" ]
                , StrE () " shows up in the bottom."
                ]
            ]
        , CallE ()
            "anchor"
            [ StrE () "meet-drracket"
            , CallE ()
                "examine"
                [ CallE ()
                    "image"
                    [ StrE () "https://htdp.org/2022-8-7/Book/drracket-plain.png"
                    ]
                , CallE ()
                    "row"
                    [ CallE () "col" []
                    , CallE ()
                        "col"
                        [ CallE ()
                            "italic"
                            [ StrE () "Meet DrRacket"
                            ]
                        ]
                    , CallE () "col" []
                    ]
                ]
            ]
        , CallE ()
            "col"
            [ CallE ()
                "para"
                [ StrE () <|
                    "That's how simple programming is. You ask questions if DrRacket were a child, and\n"
                        ++ "DrRacket computes for you. You can also ask DrRacket to process several requests at once:"
                ]
            , CallE ()
                "code-block"
                [ StrE () "(+ 2 2)"
                , StrE () "(* 3 3)"
                , StrE () "(- 4 2)"
                , StrE () "(/ 6 2)"
                ]
            , CallE ()
                "para"
                [ StrE () "After you click "
                , CallE ()
                    "italic"
                    [ StrE () "RUN"
                    ]
                , StrE () ", you see "
                , CallE ()
                    "code"
                    [ StrE () "4 9 2 3"
                    ]
                , StrE () <|
                    " in the bottom half of DrRacket,\n"
                        ++ "which are the expected results.\n"
                ]
            ]
        , CallE ()
            "col"
            [ CallE ()
                "para"
                [ StrE () "Let's slow down for a moment and introduce some words:"
                ]
            , CallE ()
                "bullet-list"
                [ CallE ()
                    "para"
                    [ StrE () "The top half or DrRacket is called the "
                    , CallE ()
                        "term"
                        [ StrE () "definitions area"
                        ]
                    , StrE () ". In this area, you create programs, which is called "
                    , CallE ()
                        "term"
                        [ StrE () "editing"
                        ]
                    , StrE () ". As soon as you add a word or change something in the definitions area, the "
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () " button shows up in the top-left corner. When you click "
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () <|
                        " for the first time, DrRacket asks you for the name of a file so that "
                            ++ "it can store your program for good. Once your definitions area is associated with a file, "
                            ++ "clicking "
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () " ensures that the content of the definitions area is stored safely in he file"
                    ]
                , CallE ()
                    "para"
                    [ StrE () "The top half or DrRacket is called the "
                    , CallE ()
                        "term"
                        [ StrE () "definitions area"
                        ]
                    , StrE () ". In this area, you create programs, which is called"
                    , CallE ()
                        "term"
                        [ StrE () "editing"
                        ]
                    , StrE () ". As soon as you add a word or change something in the definitions area, the"
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () " button shows up in the top-left corner. When you click"
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () <|
                        " for the first time, DrRacket asks you for the name of a file so that"
                            ++ "it can store your program for good. Once your definitions area is associated with a file,"
                            ++ "clicking "
                    , CallE ()
                        "italic"
                        [ StrE () "SAVE"
                        ]
                    , StrE () " ensures that the content of the definitions area is stored safely in the file."
                    ]
                , CallE ()
                    "para"
                    [ CallE ()
                        "italic"
                        [ StrE () "Programs"
                        ]
                    , StrE () <|
                        " consist of (italic \"expressions\"). You have seen expressions in mathematics. "
                            ++ "For now, an expression is either a plain number or something that starts with a left parenthesis "
                            ++ "\"(\" and ends in a matching right parenthesis \")\" "
                    , NameE () "emdash"
                    , StrE () " which DrRacket rewards by shading the area between the pair of parentheses."
                    ]
                , CallE ()
                    "col"
                    [ CallE ()
                        "para"
                        [ StrE () "When you click "
                        , CallE ()
                            "italic"
                            [ StrE () "RUN"
                            ]
                        , StrE () ", DrRacket evaluates the expressions in the definitions area and shows their result in the "
                        , CallE ()
                            "term"
                            [ StrE () "interactions area"
                            ]
                        , StrE () ". Then, DrRacket, your faithful servant, awaits your commands at the "
                        , CallE ()
                            "term"
                            [ StrE () "prompt"
                            ]
                        , StrE () <|
                            " (>). The appearance of the prompt signals that "
                                ++ "DrRacket is waiting for you to enter additional expressoins, which it then evaluates like "
                                ++ "those in the definitions area: "
                        ]
                    , CallE ()
                        "code-block"
                        [ StrE () "> (+ 1 1)"
                        , StrE () "2"
                        ]
                    , CallE ()
                        "para"
                        [ StrE () <|
                            "Enter an expression at the prompt, hit the \"return\" or \"enter\" key on your keyboard, and "
                                ++ "watch how DrRacket responds with the result. You can do so as often as you wish:"
                        ]
                    , CallE ()
                        "code-block"
                        [ StrE () "> (+ 2 2)"
                        , StrE () "4"
                        , StrE () "> (* 3 3)"
                        , StrE () "9"
                        , StrE () "> (- 4 2)"
                        , StrE () "2"
                        , StrE () "> (/ 6 2)"
                        , StrE () "3"
                        , StrE () "> (sqr 3)"
                        , StrE () "9"
                        , StrE () "> (expt 2 3)"
                        , StrE () "8"
                        , StrE () "> (sin 0)"
                        , StrE () "0"
                        , StrE () "> (cos pi)"
                        , StrE () "#i-1.0"
                        ]
                    ]
                ]
            ]
        , CallE ()
            "para"
            [ StrE () "Take a close look at the last number. Its "
            , CallE ()
                "code"
                [ StrE () "#i"
                ]
            , StrE () " prefix is short for \"I don't really know the precise number so take that for now\" or an"
            , CallE ()
                "term"
                [ StrE () "inexact number"
                ]
            , StrE () <|
                ". Unlike your calculator"
                    ++ "or other programming systems, DrRacket is honest. When it doesn't know the exact number, it "
                    ++ "warns you with this special prefix. Later, we will show you really strange facts about "
                    ++ "\"computer numbers\", and you will then truly appreciate that DrRacket issues such warnings. "
            ]
        , CallE ()
            "col"
            [ CallE ()
                "para"
                [ StrE () <|
                    "By now you might be wondering whether DrRacket can add more than two numbers at once, "
                        ++ "and yes, it can! As a matter of fact, it can do it in two different ways:"
                ]
            , CallE ()
                "code-block"
                [ StrE () "> (+ 2 (+ 3 4))"
                , StrE () "9"
                , StrE () "> (+ 2 3 4)"
                , StrE () "9"
                ]
            , CallE ()
                "para"
                [ StrE () "The first one is "
                , CallE ()
                    "term"
                    [ StrE () "nested arithmetic"
                    ]
                , StrE () ", as you know it from school. The second one is the"
                , CallE ()
                    "term"
                    [ StrE () "BSL arithmetic"
                    ]
                , StrE () <|
                    "; and the latter is natural, because in this notation you always use "
                        ++ "parentheses to group operations and numbers together."
                ]
            ]
        , CallE ()
            "col"
            [ CallE ()
                "para"
                [ StrE () <|
                    "In BSL, every time you want to use a \"calculator operation\", you write down an opening "
                        ++ "parenthesis, the operation you wish to perform, say "
                , CallE ()
                    "code"
                    [ StrE () "+"
                    ]
                , StrE () <|
                    ", the numbers on which the "
                        ++ "operation should work (separated by spaces or even line breaks), and, finally, a closing "
                        ++ "parenthesis. The items following the operation are called the "
                , CallE ()
                    "term"
                    [ StrE () "operands"
                    ]
                , StrE () ". Nested arithmetic means that you can use an expression for an operand, which is why"
                ]
            , CallE ()
                "code-block"
                [ StrE () "> (+ 2 (+ 3 4))"
                , StrE () "9"
                ]
            , CallE ()
                "para"
                [ StrE () "is a fine program. You can do this as often as you wish:"
                ]
            , CallE ()
                "code-block"
                [ StrE () "> (+ 2 (+ (* 3 3) 4))"
                , StrE () "15"
                , StrE () "> (+ 2 (+ (* 3 (/ 12 4)) 4))"
                , StrE () "15"
                , StrE () "> (+ (* 5 5) (+ (* 3 (/ 12 4)) 4))"
                , StrE () "38"
                ]
            , CallE ()
                "para"
                [ StrE () "There are no limits to nesting, except for your patience."
                ]
            ]
        ]
    ]


init : Model
init =
    { items =
        Array.fromList sample
    , selection = Nothing
    }


type Msg
    = ItemSelected Pos
    | MoveAfter { original : Int, after : Int }


update : Msg -> Model -> Model
update msg ({ selection, items } as model) =
    case msg of
        ItemSelected idx ->
            { model
                | selection =
                    if selection == Just idx then
                        Nothing

                    else
                        Just idx
            }

        MoveAfter { original, after } ->
            { model
                | items =
                    case items |> Array.get original of
                        Just item ->
                            if original < after then
                                items |> Ae.removeAt original |> Ae.insertAt after item

                            else
                                items |> Ae.removeAt original |> Ae.insertAt (after + 1) item

                        Nothing ->
                            items
                , selection = Nothing
            }


viewHtml : Model -> Html.Html Msg
viewHtml model =
    E.layout [ E.padding Ui.sizeM ] (view model)


debugExpr : Expr -> String
debugExpr expr =
    case expr of
        Lp.StrE _ s ->
            Je.encode 0 <| Je.string s

        Lp.CallE _ name args ->
            "(" ++ name ++ " " ++ String.join " " (List.map debugExpr args) ++ ")"

        Lp.NameE _ name ->
            name


treeifyStrNode : String -> E.Element Msg
treeifyStrNode s =
    E.el
        [ Border.rounded Ui.sizeS
        , Border.color Ui.colorTextNodeDark
        , Border.width 1
        , E.paddingXY Ui.sizeMs Ui.sizeMs
        , Bg.color Ui.colorTextNode
        , Font.color (E.rgb255 255 255 255)
        , E.width (E.shrink |> E.maximum 600)
        ]
        (E.column [ E.spacing Ui.sizeS ]
            (s
                |> String.split "\n"
                |> List.map
                    (\line -> E.paragraph [] [ E.text line ])
            )
        )


treeifyNameNode : String -> E.Element Msg
treeifyNameNode s =
    E.el
        [ Border.rounded Ui.sizeM
        , Border.color Ui.colorIdentifierNodeDark
        , Border.width 1
        , E.paddingXY Ui.sizeMs Ui.sizeS
        , Bg.color Ui.colorIdentifierNode
        , Font.color (E.rgb255 255 255 255)
        ]
        (E.text s)


treeifyCallNode : String -> List Expr -> E.Element Msg
treeifyCallNode name args =
    E.wrappedRow
        [ Bg.color (Ui.colorMain |> E.toRgb |> (\c -> { c | alpha = 0.2 }) |> E.fromRgb)
        , Border.rounded Ui.sizeM
        , Border.color Ui.colorMainInset
        , Border.widthEach
            { top = 0
            , bottom = 0
            , left = 1
            , right = 0
            }
        , Ui.backShadeDark
        , E.paddingXY Ui.sizeMs Ui.sizeS
        , E.spacing Ui.sizeMs
        , E.width E.fill
        , E.alignTop
        ]
        [ E.el
            [ E.alignTop
            , E.padding Ui.sizeS
            , Font.color Ui.colorMainDark
            , Border.rounded Ui.sizeL
            , Border.glow (Ui.colorMainLight |> E.toRgb |> (\c -> { c | alpha = 0.2 }) |> E.fromRgb) 3
            , Bg.color (Ui.colorMainLight |> E.toRgb |> (\c -> { c | alpha = 0.2 }) |> E.fromRgb)
            ]
            (E.text name)
        , E.column
            [ E.spacing Ui.sizeMs
            , E.alignTop
            , E.width E.fill
            ]
            (List.map treeifyExpr args)
        ]


treeifyExpr : Expr -> E.Element Msg
treeifyExpr expr =
    case expr of
        Lp.StrE _ s ->
            treeifyStrNode s

        Lp.NameE _ name ->
            treeifyNameNode name

        Lp.CallE _ name args ->
            treeifyCallNode name args


view : Model -> E.Element Msg
view { items, selection } =
    E.el
        [ Border.color Ui.colorMainDark
        , Border.width 1
        , Ui.backShadeDark
        , Bg.color Ui.colorMainLight
        , E.width E.fill
        , E.height (E.shrink |> E.minimum 60)
        ]
    <|
        E.column
            [ E.padding Ui.sizeS
            , E.spacing Ui.sizeS
            ]
            (Array.toList <| Array.map treeifyExpr items)
