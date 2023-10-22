type t = C : Modifier.t list * 'a Style.t * 'a Style.arg -> t

let make : type a. ?modifiers:Modifier.t list -> a Style.t -> a Style.arg -> t =
 fun ?(modifiers = []) t a -> C (modifiers, t, a)

let apply_modifier modifier (C (modifiers, s, a)) =
  C (modifier :: modifiers, s, a)

let to_string (C (modifiers, style, arg)) =
  let style_str_opt = Style.to_string style in
  Format.asprintf "%a%a%a"
    (fun fmt () ->
      List.iter
        (fun modifier ->
          Format.pp_print_string fmt (Modifier.to_string modifier);
          Format.pp_print_char fmt ':')
        modifiers)
    ()
    (Format.pp_print_option Format.pp_print_string)
    style_str_opt
    (fun fmt () ->
      match arg with
      | Style.Term -> ()
      | _ ->
          Option.iter (fun _ -> Format.pp_print_char fmt '-') style_str_opt;
          Format.pp_print_string fmt (Style.arg_to_string arg))
    ()
