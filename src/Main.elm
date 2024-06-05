module Main exposing (..)

import Browser exposing (sandbox)
import Color exposing (Color, pickColor)
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (style)
import Pattern exposing (..)
import Json.Decode as JD
import Html exposing (textarea)
import Html.Events exposing (onInput)
import Html exposing (p)


-- MAIN


main : Program () Model Msg
main =
    sandbox
        { init = init
        , view = view
        , update = update
        }



-- MODEL


type alias Model = { patterns : List Pattern, patternsParseError: String }


init : Model
init =
    { patterns =
        [ { nodes =
                [ Node LITERAL 1 ['+']
                , Node LITERAL 1 ['7']
                , Node LITERAL 1 ['9']
                , Node DIGITS 9 []
                ]
          , percentage = 0.455
          }
        , { nodes =
                [ Node LITERAL 1 ['8']
                , Node LITERAL 1 [' ']
                , Node LITERAL 1 ['(']
                , Node LITERAL 1 ['9']
                , Node DIGITS 2 []
                , Node LITERAL 1 [')']
                , Node LITERAL 1 [' ']
                , Node DIGITS 3 []
                , Node LITERAL 1 ['-']
                , Node DIGITS 2 []
                , Node LITERAL 1 ['-']
                , Node DIGITS 2 []
                ]
          , percentage = 0.105
          }
        , { nodes =
                [ Node RU_UPPER 1 []
                , Node DIGITS 3 []
                , Node RU_UPPER 2 []
                ]
          , percentage = 0.0069
          }
        ]
    , patternsParseError = ""
    }



-- UPDATE


type Msg = UpdatePatterns String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdatePatterns json -> case JD.decodeString pattersDecoder json of
            Ok patterns -> { model | patterns = patterns, patternsParseError = "" }
            Err error -> { model | patternsParseError = JD.errorToString error }


-- VIEW


fontSize : String
fontSize = "24pt"


fontFamily: String
fontFamily = "Monospace"


nodeBox : String -> Color -> Html Msg
nodeBox nodeText color =
    span
        [ style "background-color" color.light
        , style "border" ("solid 3px " ++ color.dark)
        , style "border-radius" "5pt"
        , style "font-size" fontSize
        , style "font-family" fontFamily
        ]
        [ text nodeText ]


viewNode : Color -> Node -> Html Msg
viewNode color { symbolSet, quantifier, symbols } =
    let
        quantifierText =
            if quantifier > 1 then
                "{" ++ String.fromInt quantifier ++ "}"
            else
                ""

        nodeSymbolText =
            case symbolSet of
                LITERAL -> case List.head symbols of
                    Just char -> String.fromChar char
                    Nothing -> "error"
                _ ->
                    symbolRegex symbolSet
    in
    nodeBox (nodeSymbolText ++ quantifierText) color


viewNodes : List Node -> Html Msg
viewNodes nodes =
    div [] <| List.indexedMap (pickColor >> viewNode) nodes


viewPercentage : Float -> Html Msg
viewPercentage percentage =
    span
        [ style "font-size" fontSize
        , style "font-family" fontFamily
        ]
        [ text <| String.fromFloat (percentage * 100) ++ "%" ]


viewPattern : Pattern -> Html Msg
viewPattern { nodes, percentage } =
    div
        [ style "display" "flex"
        , style "gap" "24pt"
        ]
        [ viewPercentage percentage
        , viewNodes nodes
        ]


viewPatterns : List Pattern -> Html Msg
viewPatterns patterns =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "gap" "24pt"
        , style "padding" "12pt"
        , style "width" "content"
        ]
    <|
        List.map viewPattern patterns


view : Model -> Html Msg
view { patterns, patternsParseError } =
    div [ style "display" "flex"
        , style "align-items" "stretch"
        , style "gap" "24pt"
        , style "padding" "12pt"
        ]
        [ div [ style "width" "40%", style "height" "50vh" ]
            [ textarea [ onInput UpdatePatterns, style "width" "100%", style "height" "50%"] []
            , p [ style "color" "red" ] [text patternsParseError]
            ]
        , viewPatterns patterns
        ]
