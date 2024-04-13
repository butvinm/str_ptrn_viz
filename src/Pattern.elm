module Pattern exposing (..)


type SymbolSet
    = SYMBOL
    | LITERAL Char
    | WORD
    | DIGIT
    | ALPHA
    | EN
    | RU
    | EN_UPPER
    | RU_UPPER
    | EN_LOWER
    | RU_LOWER


symbolRegex : SymbolSet -> String
symbolRegex symbolSet =
    case symbolSet of
        SYMBOL   -> "punct|space"
        WORD     -> "alnum"
        ALPHA    -> "alpha"
        EN       -> "[A-Za-z]"
        EN_UPPER -> "[A-Z]"
        EN_LOWER -> "[a-z]"
        RU       -> "[А-Яа-я]"
        RU_UPPER -> "[А-Я]"
        RU_LOWER -> "[а-я]"
        DIGIT    -> "digit"
        _        -> "error"


type alias Node = { symbolSet : SymbolSet, quantifier : Int }


type alias Pattern = { nodes : List Node, frequency : Float }
