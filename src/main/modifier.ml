type t = 
  | Sm
  | Md
  | Lg
  | Xl
  | Xxl
  | Dark
  | Hover

let to_string = function
  | Sm -> "sm"
  | Md -> "md"
  | Lg -> "lg"
  | Xl -> "xl"
  | Xxl -> "2xl"
  | Dark -> "dark"
  | Hover -> "hover"
