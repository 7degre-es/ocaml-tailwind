let ( !! ) v = v Style.Term

(** Styles *)

let aspect a = Klass.make Aspect a
let flex a = Klass.make Flex a
let font a = Klass.make Font a
let text a = Klass.make Text a
let tracking a = Klass.make Tracking a
let list a = Klass.make List a
let decoration a = Klass.make Decoration a
let justify a = Klass.make Justify a
let justify_items a = Klass.make Justify_items a
let background a = Klass.make Background a

(** Compound styles *)

let antialiased = Klass.make Smoothing Antialiased
let subpixel_antialiased = Klass.make Smoothing Subpixel_antialiased
let italic = Klass.make Font_style Italic
let not_italic = Klass.make Font_style Not_italic
let normal_nums = Klass.make Font_variant_numeric Normal_nums
let ordinal = Klass.make Font_variant_numeric Ordinal
let slashed_zero = Klass.make Font_variant_numeric Slashed_zero
let lining_nums = Klass.make Font_variant_numeric Lining_nums
let oldstyle_nums = Klass.make Font_variant_numeric Oldstyle_nums
let proportional_nums = Klass.make Font_variant_numeric Proportional_nums
let tabular_nums = Klass.make Font_variant_numeric Tabular_nums
let diagonal_fractions = Klass.make Font_variant_numeric Diagonal_fractions
let stacked_fractions = Klass.make Font_variant_numeric Stacked_fractions
let underline = Klass.make Text_decoration Underline
let overline = Klass.make Text_decoration Overline
let line_through = Klass.make Text_decoration Line_through
let no_underline = Klass.make Text_decoration No_underline
let uppercase = Klass.make Text_transform Uppercase
let lowercase = Klass.make Text_transform Lowercase
let capitalize = Klass.make Text_transform Uppercase
let normal_case = Klass.make Text_transform Normal_case

(** Colors *)

let slate t = Style.Slate t
let gray t = Style.Gray t
let zinc t = Style.Zinc t
let neutral t = Style.Neutral t
let stone t = Style.Stone t
let red t = Style.Red t
let orange t = Style.Orange t
let amber t = Style.Amber t
let yellow t = Style.Yellow t
let lime t = Style.Lime t
let green t = Style.Green t
let emerald t = Style.Emerald t
let teal t = Style.Teal t
let cyan t = Style.Cyan t
let sky t = Style.Sky t
let blue t = Style.Blue t
let indigo t = Style.Indigo t
let violet t = Style.Violet t
let purple t = Style.Purple t
let fuschia t = Style.Fuschia t
let pink t = Style.Pink t
let rose t = Style.Rose t

(** Args *)

let none = Style.None
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
let tighter = Style.Tighter
let tight = Style.Tight
let wide = Style.Wide
let wider = Style.Wider
let widest = Style.Widest
let inside = Style.Inside
let outside = Style.Outside
let disc = Style.Disc
let decimal = Style.Decimal
let left = Style.Left
let center = Style.Center
let right = Style.Right
let justify_ = Style.Justify
let start = Style.Start
let end_ = Style.End
let solid = Style.Solid
let double = Style.Double
let dotted = Style.Dotted
let dashed = Style.Dashed
let wavy = Style.Wavy
let between = Style.Between
let around = Style.Around
let evenly = Style.Evenly
let stretch = Style.Stretch

(** Modifiers *)

let sm' = Klass.apply_modifier Sm
let md' = Klass.apply_modifier Md
let lg' = Klass.apply_modifier Lg
let xl' = Klass.apply_modifier Xl
let xxl' = Klass.apply_modifier Xxl
let hover' = Klass.apply_modifier Hover
let dark' = Klass.apply_modifier Dark
