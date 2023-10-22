type _ t =
  | Flex : [ `flex ] t
  | Width : [ `width ] t
  | Font : [ `font ] t
  | Text : [ `text ] t
  | Aspect : [ `aspect ] t
  | Smoothing : [ `smoothing ] t
  | Font_style : [ `font_style ] t
  | Font_variant_numeric : [ `font_variant_numeric ] t
  | Tracking : [ `tracking ] t
  | List : [ `list ] t
  | Text_decoration : [ `text_decoration ] t
  | Decoration : [ `decoration ] t
  | Text_transform : [ `text_transform ] t
  | Justify : [ `justify ] t
  | Justify_items : [ `justify_items ] t
  | Background : [ `background ] t

let to_string : type a. a t -> string option = function
  | Aspect -> Some "aspect"
  | Flex -> Some "flex"
  | Width -> Some "width"
  | Font -> Some "font"
  | Text -> Some "text"
  | Tracking -> Some "tracking"
  | List -> Some "list"
  | Decoration -> Some "decoration"
  | Justify -> Some "justify"
  | Justify_items -> Some "justify-items"
  | Background -> Some "bg"
  | Font_style | Smoothing | Font_variant_numeric | Text_decoration
  | Text_transform ->
      None

type dimension =
  [ `width | `min_width | `max_width | `height | `min_height | `max_height ]

type color = [ `background ]

type _ arg =
  | Term : [< `flex ] arg
  | Arbitrary : string -> _ arg
  | Custom : string -> _ arg
  | Auto : [< `aspect ] arg
  | Square : [< `aspect ] arg
  | Video : [< `aspect ] arg
  | Inherit : [< `background ] arg
  | Current : [< `background ] arg
  | Transparent : [< color ] arg
  | White : [< color ] arg
  | Slate : Color.temperature -> [< color ] arg
  | Gray : Color.temperature -> [< color ] arg
  | Zinc : Color.temperature -> [< color ] arg
  | Neutral : Color.temperature -> [< color ] arg
  | Stone : Color.temperature -> [< color ] arg
  | Red : Color.temperature -> [< color ] arg
  | Orange : Color.temperature -> [< color ] arg
  | Amber : Color.temperature -> [< color ] arg
  | Yellow : Color.temperature -> [< color ] arg
  | Lime : Color.temperature -> [< color ] arg
  | Green : Color.temperature -> [< color ] arg
  | Emerald : Color.temperature -> [< color ] arg
  | Teal : Color.temperature -> [< color ] arg
  | Cyan : Color.temperature -> [< color ] arg
  | Sky : Color.temperature -> [< color ] arg
  | Blue : Color.temperature -> [< color ] arg
  | Indigo : Color.temperature -> [< color ] arg
  | Violet : Color.temperature -> [< color ] arg
  | Purple : Color.temperature -> [< color ] arg
  | Fuschia : Color.temperature -> [< color ] arg
  | Pink : Color.temperature -> [< color ] arg
  | Rose : Color.temperature -> [< color ] arg
  | Solid : [< `decoration ] arg
  | Double : [< `decoration ] arg
  | Dotted : [< `decoration ] arg
  | Dashed : [< `decoration ] arg
  | Wavy : [< `decoration ] arg
  | Full : [< dimension ] arg
  | Sans : [< `font ] arg
  | Serif : [< `font ] arg
  | Mono : [< `font ] arg
  | Thin : [< `font ] arg
  | Extralight : [< `font ] arg
  | Light : [< `font ] arg
  | Normal : [< `font | `tracking | `justify ] arg
  | Medium : [< `font ] arg
  | Semibold : [< `font ] arg
  | Bold : [< `font ] arg
  | Extrabold : [< `font ] arg
  | Black : [< `font | color ] arg
  | Italic : [< `font_style ] arg
  | Not_italic : [< `font_style ] arg
  | Normal_nums : [< `font_variant_numeric ] arg
  | Ordinal : [< `font_variant_numeric ] arg
  | Slashed_zero : [< `font_variant_numeric ] arg
  | Lining_nums : [< `font_variant_numeric ] arg
  | Oldstyle_nums : [< `font_variant_numeric ] arg
  | Proportional_nums : [< `font_variant_numeric ] arg
  | Tabular_nums : [< `font_variant_numeric ] arg
  | Diagonal_fractions : [< `font_variant_numeric ] arg
  | Stacked_fractions : [< `font_variant_numeric ] arg
  | Row : [< `flex ] arg
  | Row_reverse : [< `flex ] arg
  | Col : [< `flex ] arg
  | Col_reverse : [< `flex ] arg
  | Wrap : [< `flex ] arg
  | Wrap_reverse : [< `flex ] arg
  | Nowrap : [< `flex ] arg
  | Between : [< `justify ] arg
  | Around : [< `justify ] arg
  | Evenly : [< `justify ] arg
  | Stretch : [< `justify | `justify_items ] arg
  | Inside : [< `list ] arg
  | Outside : [< `list ] arg
  | None : [< `list ] arg
  | Disc : [< `list ] arg
  | Decimal : [< `list ] arg
  | Antialiased : [< `smoothing ] arg
  | Subpixel_antialiased : [< `smoothing ] arg
  | Xs : [< `text ] arg
  | Sm : [< `text ] arg
  | Base : [< `text ] arg
  | Lg : [< `text ] arg
  | Xl : [< `text ] arg
  | X2l : [< `text ] arg
  | X3l : [< `text ] arg
  | X4l : [< `text ] arg
  | X5l : [< `text ] arg
  | X6l : [< `text ] arg
  | X7l : [< `text ] arg
  | X8l : [< `text ] arg
  | X9l : [< `text ] arg
  | Left : [< `text ] arg
  | Center : [< `text | `justify | `justify_items ] arg
  | Right : [< `text ] arg
  | Justify : [< `text ] arg
  | Start : [< `text | `justify | `justify_items ] arg
  | End : [< `text | `justify | `justify_items ] arg
  | Underline : [< `text_decoration ] arg
  | Overline : [< `text_decoration ] arg
  | Line_through : [< `text_decoration ] arg
  | No_underline : [< `text_decoration ] arg
  | Uppercase : [< `text_transform ] arg
  | Lowercase : [< `text_transform ] arg
  | Capitalize : [< `text_transform ] arg
  | Normal_case : [< `text_transform ] arg
  | Tighter : [< `tracking ] arg
  | Tight : [< `tracking ] arg
  | Wide : [< `tracking ] arg
  | Wider : [< `tracking ] arg
  | Widest : [< `tracking ] arg

let arg_to_string : type a. a arg -> string = function
  | Arbitrary str -> "[" ^ str ^ "]"
  | Auto -> "auto"
  | Col -> "col"
  | Col_reverse -> "col-reverse"
  | Custom str -> str
  | Full -> "full"
  | Nowrap -> "nowrap"
  | Row -> "row"
  | Row_reverse -> "row-reverse"
  | Square -> "square"
  | Term -> "term"
  | Video -> "video"
  | Wrap -> "wrap"
  | Wrap_reverse -> "wrap-reverse"
  | Sans -> "sans"
  | Serif -> "serif"
  | Mono -> "mono"
  | Xs -> "xs"
  | Sm -> "sm"
  | Base -> "base"
  | Lg -> "lg"
  | Xl -> "xl"
  | X2l -> "2xl"
  | X3l -> "3xl"
  | X4l -> "4xl"
  | X5l -> "5xl"
  | X6l -> "6xl"
  | X7l -> "7xl"
  | X8l -> "8xl"
  | X9l -> "9xl"
  | Thin -> "thin"
  | Extralight -> "extralight"
  | Light -> "light"
  | Normal -> "normal"
  | Medium -> "medium"
  | Semibold -> "semibold"
  | Bold -> "bold"
  | Extrabold -> "extrabold"
  | Black -> "black"
  | Antialiased -> "antialiased"
  | Subpixel_antialiased -> "subpixel-antialiased"
  | Italic -> "italic"
  | Not_italic -> "not-italic"
  | Normal_nums -> "normal-nums"
  | Ordinal -> "ordinal"
  | Slashed_zero -> "slashed-zero"
  | Lining_nums -> "lining-nums"
  | Oldstyle_nums -> "oldstyle-nums"
  | Proportional_nums -> "proportional-nums"
  | Tabular_nums -> "tabular-nums"
  | Diagonal_fractions -> "diagonal-fractions"
  | Stacked_fractions -> "stacked-fractions"
  | Tighter -> "tighter"
  | Tight -> "tight"
  | Wide -> "wide"
  | Wider -> "wider"
  | Widest -> "widest"
  | Inside -> "inside"
  | Outside -> "outside"
  | None -> "none"
  | Disc -> "disc"
  | Decimal -> "decimal"
  | Left -> "left"
  | Center -> "center"
  | Right -> "right"
  | Justify -> "justify"
  | Start -> "start"
  | End -> "end"
  | Underline -> "underline"
  | Overline -> "overline"
  | Line_through -> "line-through"
  | No_underline -> "no-underline"
  | Solid -> "solid"
  | Double -> "double"
  | Dotted -> "dotted"
  | Dashed -> "dashed"
  | Wavy -> "wavy"
  | Uppercase -> "uppercase"
  | Lowercase -> "lowercase"
  | Capitalize -> "capitalize"
  | Normal_case -> "normal-case"
  | Between -> "between"
  | Around -> "around"
  | Evenly -> "evenly"
  | Stretch -> "stretch"
  | Inherit -> "inherit"
  | Current -> "current"
  | Transparent -> "transparent"
  | White -> "white"
  | Slate t -> "slate-" ^ Color.temperature_to_string t
  | Gray t -> "gray-" ^ Color.temperature_to_string t
  | Zinc t -> "zinc-" ^ Color.temperature_to_string t
  | Neutral t -> "neutral-" ^ Color.temperature_to_string t
  | Stone t -> "stone-" ^ Color.temperature_to_string t
  | Red t -> "red-" ^ Color.temperature_to_string t
  | Orange t -> "orange-" ^ Color.temperature_to_string t
  | Amber t -> "amber-" ^ Color.temperature_to_string t
  | Yellow t -> "yellow-" ^ Color.temperature_to_string t
  | Lime t -> "lime-" ^ Color.temperature_to_string t
  | Green t -> "green-" ^ Color.temperature_to_string t
  | Emerald t -> "emerald-" ^ Color.temperature_to_string t
  | Teal t -> "teal-" ^ Color.temperature_to_string t
  | Cyan t -> "cyan-" ^ Color.temperature_to_string t
  | Sky t -> "sky-" ^ Color.temperature_to_string t
  | Blue t -> "blue-" ^ Color.temperature_to_string t
  | Indigo t -> "indigo-" ^ Color.temperature_to_string t
  | Violet t -> "violet-" ^ Color.temperature_to_string t
  | Purple t -> "purple-" ^ Color.temperature_to_string t
  | Fuschia t -> "fuschia-" ^ Color.temperature_to_string t
  | Pink t -> "pink-" ^ Color.temperature_to_string t
  | Rose t -> "rose-" ^ Color.temperature_to_string t
