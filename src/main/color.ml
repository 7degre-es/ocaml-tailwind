type temperature = int

let temperature_of_int = function
  | (50 | 100 | 200 | 300 | 400 | 500 | 600 | 700 | 800 | 900 | 950) as i ->
      Some i
  | _ -> None

let temperature_of_string x = Option.bind (int_of_string_opt x) temperature_of_int

let temperature_to_string = string_of_int
