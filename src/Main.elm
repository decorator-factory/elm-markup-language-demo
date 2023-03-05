module Main exposing (main)

import Browser
import Element as E
import Element.Font as Font
import Element.Input as Input
import Parser
import Pts


type alias Model =
    { draft : String, expr : String, tokens : String }


main : Program () Model String
main =
    Browser.sandbox
        { init = { draft = "", tokens = "", expr = "" }
        , view = view >> E.layout [ E.width E.fill ]
        , update =
            \msg model ->
                { model
                    | draft = msg
                    , expr = Debug.toString <| Parser.run Pts.program msg
                    , tokens = Debug.toString <| Parser.run Pts.tokenize msg
                }
        }


view : Model -> E.Element String
view { draft, expr, tokens } =
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
        , E.paragraph [ E.width (E.px 500) ] [ E.text expr ]
        , E.paragraph [ E.width (E.px 500) ] [ E.text tokens ]
        ]
