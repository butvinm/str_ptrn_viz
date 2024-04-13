module Main exposing (..)

import Browser exposing (sandbox)
import Color exposing (Color, pickColor)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Pattern exposing (..)



-- MAIN


main : Program () Model msg
main =
    sandbox
        { init = init
        , view = view
        , update = \_ model -> model
        }



-- MODEL


type alias Model = { patterns : List Pattern }


init : Model
init =
    { patterns =
        [ { nodes =
                [ Node (LITERAL '+') 1
                , Node (LITERAL '7') 1
                , Node (LITERAL '9') 1
                , Node DIGIT 9
                ]
          , frequency = 45.5
          }
        , { nodes =
                [ Node (LITERAL '8') 1
                , Node (LITERAL ' ') 1
                , Node (LITERAL '(') 1
                , Node (LITERAL '9') 1
                , Node DIGIT 2
                , Node (LITERAL ')') 1
                , Node (LITERAL ' ') 1
                , Node DIGIT 3
                , Node (LITERAL '-') 1
                , Node DIGIT 2
                , Node (LITERAL '-') 1
                , Node DIGIT 2
                ]
          , frequency = 10.5
          }
        , { nodes =
                [ Node RU_UPPER 1
                , Node DIGIT 3
                , Node RU_UPPER 2
                ]
          , frequency = 0.69
          }
        ]
    }


-- VIEW


fontSize : String
fontSize = "24pt"


fontFamily: String
fontFamily = "Monospace"


nodeBox : String -> Color -> Html msg
nodeBox nodeText color =
    span
        [ style "background-color" color.light
        , style "border" ("solid 3px " ++ color.dark)
        , style "border-radius" "5pt"
        , style "font-size" fontSize
        , style "font-family" fontFamily
        ]
        [ text nodeText ]


viewNode : Color -> Node -> Html msg
viewNode color { symbolSet, quantifier } =
    let
        quantifierText =
            if quantifier > 1 then
                "{" ++ String.fromInt quantifier ++ "}"
            else
                ""

        nodeSymbolText =
            case symbolSet of
                LITERAL symbol ->
                    String.fromChar (if symbol == ' ' then '\u{00A0}' else symbol)
                _ ->
                    symbolRegex symbolSet
    in
    nodeBox (nodeSymbolText ++ quantifierText) color


viewNodes : List Node -> Html msg
viewNodes nodes =
    div [] <| List.indexedMap (pickColor >> viewNode) nodes


viewFrequency : Float -> Html msg
viewFrequency frequency =
    span
        [ style "font-size" fontSize
        , style "font-family" fontFamily
        ]
        [ text <| String.fromFloat frequency ++ "%" ]


viewPattern : Pattern -> Html msg
viewPattern { nodes, frequency } =
    div
        [ style "display" "flex"
        , style "gap" "24pt"
        ]
        [ viewFrequency frequency
        , viewNodes nodes
        ]


view : Model -> Html msg
view { patterns } =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "24pt"
        , style "padding" "12pt"
        ]
    <|
        List.map viewPattern patterns
