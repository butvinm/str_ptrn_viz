module Pattern exposing (..)

import Json.Decode as JD


type SymbolSet
    = LITERAL
    | WORD
    | DIGITS
    | EN
    | EN_UPPER
    | EN_LOWER
    | RU
    | RU_UPPER
    | RU_LOWER


symbolRegex : SymbolSet -> String
symbolRegex symbolSet =
    case symbolSet of
        WORD     -> "\\w"
        EN       -> "[A-Za-z]"
        EN_UPPER -> "[A-Z]"
        EN_LOWER -> "[a-z]"
        RU       -> "[А-Яа-яЁё]"
        RU_UPPER -> "[А-ЯЁ]"
        RU_LOWER -> "[а-яё]"
        DIGITS   -> "[0-9]"
        _        -> "error"


type alias Node = { symbolSet : SymbolSet, quantifier : Int, symbols: List Char }


type alias Pattern = { nodes : List Node, percentage : Float }


symbolSetDecoder : JD.Decoder SymbolSet
symbolSetDecoder =
    let
        strToSymbolSet = \str -> case str of
            "LITERAL"  -> JD.succeed LITERAL
            "WORD"     -> JD.succeed WORD
            "DIGITS"   -> JD.succeed DIGITS
            "EN"       -> JD.succeed EN
            "RU"       -> JD.succeed RU
            "EN_UPPER" -> JD.succeed EN_UPPER
            "RU_UPPER" -> JD.succeed RU_UPPER
            "EN_LOWER" -> JD.succeed EN_LOWER
            "RU_LOWER" -> JD.succeed RU_LOWER
            _          -> JD.fail "Unknown symbol set"
    in
    JD.string |> JD.andThen strToSymbolSet


nodeSymbolDecoder : JD.Decoder Char
nodeSymbolDecoder =
    let
        strToChar = \str -> case String.uncons str of
            Just (head, _) -> JD.succeed head
            Nothing -> JD.fail "Is not a char"
    in
    JD.string |> JD.andThen strToChar


nodeDecoder : JD.Decoder Node
nodeDecoder =
    JD.map3 Node
        (JD.field "symbolSet" symbolSetDecoder)
        (JD.field "quantifier" JD.int)
        (JD.field "symbols" (JD.list nodeSymbolDecoder))


patterDecoder : JD.Decoder Pattern
patterDecoder =
    JD.map2 Pattern
        (JD.field "nodes" (JD.list nodeDecoder))
        (JD.field "percentage" JD.float)


pattersDecoder : JD.Decoder (List Pattern)
pattersDecoder =
    JD.list patterDecoder
