module Editor.Ui exposing
    ( backShadeDark
    , colorMain
    , colorMainDark
    , colorMainLight
    , colorMainLighter
    , colorTextNode
    , sizeL
    , sizeLx
    , sizeM
    , sizeMs
    , sizeS
    , sizeX, colorIdentifierNode, colorIdentifierNodeDark, colorTextNodeDark, colorMainInset, backShadeDarkStronger
    )

import Element as E
import Element.Background as Bg
import Element.Border as Border
import Element.Font as Font


type Hsl
    = Hsla Float Float Float Float


{-| Adapted from y047aka/elm-hsl-color@1.0.2, MIT license
-}
hslToRgb : Hsl -> { red : Float, green : Float, blue : Float, alpha : Float }
hslToRgb (Hsla h s l a) =
    let
        m2 =
            if l <= 0.5 then
                l * (s + 1)

            else
                l + s - l * s

        m1 =
            l * 2 - m2

        hueToRgb h__ =
            let
                h_ =
                    if h__ < 0 then
                        h__ + 1

                    else if h__ > 1 then
                        h__ - 1

                    else
                        h__
            in
            if h_ * 6 < 1 then
                m1 + (m2 - m1) * h_ * 6

            else if h_ * 2 < 1 then
                m2

            else if h_ * 3 < 2 then
                m1 + (m2 - m1) * (2 / 3 - h_) * 6

            else
                m1
    in
    { red = hueToRgb (h + 1 / 3)
    , green = hueToRgb h
    , blue = hueToRgb (h - 1 / 3)
    , alpha = a
    }


{-|

    h : 0..360
    s : 0..100
    l : 0..100

-}
hsl : Int -> Int -> Int -> E.Color
hsl h s l =
    hsla h s l 1.0


{-|

    h : 0..360
    s : 0..100
    l : 0..100
    a : 0.0..1.0

-}
hsla : Int -> Int -> Int -> Float -> E.Color
hsla h s l a =
    E.fromRgb (hslToRgb (Hsla (toFloat h / 360) (toFloat s / 100) (toFloat l / 100) a))


colorMainLight : E.Color
colorMainLight =
    hsl 210 90 95


colorMainLighter : E.Color
colorMainLighter =
    hsl 215 90 80


colorMain : E.Color
colorMain =
    hsl 220 90 56


colorMainInset : E.Color
colorMainInset =
    hsl 220 85 65



colorMainDark : E.Color
colorMainDark =
    hsl 236 77 33


colorTextNode : E.Color
colorTextNode =
    hsl 240 0 27


colorTextNodeDark : E.Color
colorTextNodeDark =
    hsl 240 0 10


colorIdentifierNode : E.Color
colorIdentifierNode =
    hsl 27 85 35


colorIdentifierNodeDark : E.Color
colorIdentifierNodeDark =
    hsl 27 70 15


backShadeDark : E.Attr decorative msg
backShadeDark =
    Border.shadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 2
        , color = hsla 236 77 33 0.2
        }


backShadeDarkStronger : E.Attr decorative msg
backShadeDarkStronger =
    Border.shadow
        { offset = ( 0, 0 )
        , size = 1
        , blur = 4
        , color = hsla 236 77 10 0.5
        }


sizeS : number
sizeS =
    8


sizeMs : number
sizeMs =
    12


sizeM : number
sizeM =
    16


sizeL : number
sizeL =
    32


sizeLx : number
sizeLx =
    46


sizeX : number
sizeX =
    64
