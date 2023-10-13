type _ t =
  | Flex : [ `flex ] t
  | Width : [ `width ] t
  | Font : [ `font ] t
  | Text : [ `text ] t
  | Aspect : [ `aspect ] t

let to_string : type a. a t -> string = function
  | Aspect -> "aspect"
  | Flex -> "flex"
  | Width -> "width"
  | Font -> "font"
  | Text -> "text"

type dimension =
  [ `width | `min_width | `max_width | `height | `min_height | `max_height ]

type _ arg =
  | Term : [< `flex ] arg
  | Arbitrary : string -> _ arg
  | Custom : string -> _ arg
  | Auto : [< `aspect ] arg
  | Square : [< `aspect ] arg
  | Video : [< `aspect ] arg
  | Full : [< dimension ] arg
  | Sans : [< `font ] arg
  | Serif : [< `font ] arg
  | Mono : [< `font ] arg
  | Thin : [< `font ] arg
  | Extralight : [< `font ] arg
  | Light : [< `font ] arg
  | Normal : [< `font ] arg
  | Medium : [< `font ] arg
  | Semibold : [< `font ] arg
  | Bold : [< `font ] arg
  | Extrabold : [< `font ] arg
  | Black : [< `font ] arg
  | Row : [< `flex ] arg
  | Row_reverse : [< `flex ] arg
  | Col : [< `flex ] arg
  | Col_reverse : [< `flex ] arg
  | Wrap : [< `flex ] arg
  | Wrap_reverse : [< `flex ] arg
  | Nowrap : [< `flex ] arg
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
