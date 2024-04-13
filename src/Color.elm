module Color exposing (Color, pickColor)


type alias Color = { light : String, dark : String }


pickColor : Int -> Color
pickColor seed =
    let boundSeed = remainderBy 7 seed
    in case boundSeed of
        0  -> { light = "#e6dff3", dark = "#4f3874" }
        1  -> { light = "#f7dfef", dark = "#80386a" }
        2  -> { light = "#ffe2e9", dark = "#994357" }
        3  -> { light = "#ffeae3", dark = "#995a44" }
        4  -> { light = "#fff4df", dark = "#997739" }
        5  -> { light = "#fefee3", dark = "#959544" }
        6  -> { light = "#ffcccc", dark = "#990000" }
        7  -> { light = "#fcccdf", dark = "#8f0039" }
        8  -> { light = "#f0d4e9", dark = "#6d1956" }
        9  -> { light = "#e2dbeb", dark = "#412d5c" }
        10 -> { light = "#d6dce6", dark = "#1e304e" }
        11 -> { light = "#d5dade", dark = "#1c2b35" }
        _  -> { light = "#000000", dark = "#000000" }
