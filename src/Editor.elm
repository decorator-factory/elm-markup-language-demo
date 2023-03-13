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


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = viewHtml
        , update = update
        }


type alias Pos =
    Int


type alias Selection =
    { itemPos : Maybe Pos
    }


type alias Model =
    { items : Array String
    , selection : Selection
    }


init : Model
init =
    { items =
        Array.fromList
            [ "Foo"
            , "Bar"
            , "Apple"
            , "Banana of love"
            , "Char"
            ]
    , selection =
        { itemPos = Nothing
        }
    }


type Msg
    = ItemSelected Pos
    | MoveAfter { original : Pos, after : Pos }



-- pokeOut : Int -> List a -> List (Maybe a)
-- pokeOut idx =
--     List.indexedMap
--         (\i a ->
--             if i == idx then
--                 Nothing
--             else
--                 Just a
--         )
-- insertAfter : Int -> a -> List a -> List a
-- insertAfter idx a list =
--     List.take idx list ++ (a :: List.drop idx list)
-- moveAfter : { oldPos : Int, newPos : Int } -> List a -> List a
-- moveAfter { oldPos, newPos } oldList =
--     case oldList |> List.drop oldPos |> List.head of
--         Just oldItem ->
--             oldList
--                 |> pokeOut oldPos
--                 |> insertAfter newPos (Just oldItem)
--                 |> List.filterMap identity
--         Nothing ->
--             oldList


update : Msg -> Model -> Model
update msg ({ selection, items } as model) =
    case msg of
        ItemSelected idx ->
            { model
                | selection =
                    { selection
                        | itemPos =
                            if selection.itemPos == Just idx then
                                Nothing

                            else
                                Just idx
                    }
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
                , selection = { selection | itemPos = Nothing }
            }


viewHtml : Model -> Html.Html Msg
viewHtml model =
    E.layout [ E.padding Ui.sizeM ] (view model)


viewItem : Selection -> Pos -> String -> E.Element Msg
viewItem selection pos item =
    let
        ( selColor, selColorInv ) =
            if selection.itemPos == Just pos then
                ( Ui.colorMainDark, Ui.colorMain )

            else
                ( Ui.colorMain, Ui.colorMainDark )

        -- addBeforeButton =
        --     if List.member selection.itemIndex [ Nothing, Just index ] then
        --         Nothing
        --     else if index /= 0 then
        addAfterButton =
            selection.itemPos
                |> Maybe.andThen
                    (\selPos ->
                        if selPos == pos || selPos == pos + 1 then
                            Nothing

                        else
                            Just <|
                                E.el
                                    [ E.width (E.px Ui.sizeL)
                                    , E.height (E.px Ui.sizeL)
                                    , E.pointer
                                    , Events.onClick (MoveAfter { original = selPos, after = pos })
                                    , Bg.color Ui.colorMainLighter
                                    , Font.color Ui.colorMain
                                    , Border.widthEach
                                        { bottom = 1
                                        , left = 0
                                        , right = 0
                                        , top = 0
                                        }
                                    , Border.color (E.rgba 0 0 0 0)
                                    , E.mouseOver
                                        [ Border.color Ui.colorMain
                                        , Font.color Ui.colorMainDark
                                        ]
                                    , Border.rounded Ui.sizeS
                                    ]
                                <|
                                    E.el
                                        [ Font.size 20
                                        , E.centerX
                                        , E.centerY
                                        ]
                                        (E.text "⯈")
                    )

        mainButton =
            E.el
                [ Border.rounded Ui.sizeM
                , E.padding Ui.sizeMs
                , Bg.color selColor
                , Font.color Ui.colorMainLight
                , Font.size 24
                ]
            <|
                E.row [ E.spacing Ui.sizeM ]
                    [ E.el
                        [ E.paddingXY Ui.sizeMs Ui.sizeM
                        , Border.rounded Ui.sizeM
                        , Bg.color selColorInv
                        , E.pointer
                        , Events.onClick <| ItemSelected pos
                        ]
                        E.none
                    , E.text item
                    ]
    in
    E.row [ E.spacing Ui.sizeM ]
        (List.filterMap identity [ Just mainButton, addAfterButton ])


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
            [ E.padding Ui.sizeM
            , E.spacing Ui.sizeM
            ]
            (Array.toList <| Array.indexedMap (\i item -> viewItem selection i item) items)
