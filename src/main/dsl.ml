let ( !! ) v = v Style.Term

(** Styles *)

let aspect a = Klass.make Aspect a
let flex a = Klass.make Flex a
let font a = Klass.make Font a
let text a = Klass.make Text a

(** Args *)

let row = Style.Row
let row_reverse = Style.Row_reverse
let col = Style.Col
let col_reverse = Style.Col_reverse
let auto = Style.Auto
let square = Style.Square
let video = Style.Video
let full = Style.Full
let sans = Style.Sans
let serif = Style.Serif
let mono = Style.Mono
let wrap = Style.Wrap
let wrap_reverse = Style.Wrap_reverse
let nowrap = Style.Nowrap
let xs = Style.Xs
let sm = Style.Sm
let base = Style.Base
let lg = Style.Lg
let xl = Style.Xl
let x2l = Style.X2l
let x3l = Style.X3l
let x4l = Style.X4l
let x5l = Style.X5l
let x6l = Style.X6l
let x7l = Style.X7l
let x8l = Style.X8l
let x9l = Style.X9l
let thin = Style.Thin
let extralight = Style.Extralight
let light = Style.Light
let normal = Style.Normal
let medium = Style.Medium
let semibold = Style.Semibold
let bold = Style.Bold
let extrabold = Style.Extrabold
let black = Style.Black

(** Modifiers *)

let sm' = Klass.apply_modifier Sm
let md' = Klass.apply_modifier Md
let lg' = Klass.apply_modifier Lg
let xl' = Klass.apply_modifier Xl
let xxl' = Klass.apply_modifier Xxl
let hover' = Klass.apply_modifier Hover
let dark' = Klass.apply_modifier Dark
