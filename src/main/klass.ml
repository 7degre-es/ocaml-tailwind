type t = C : Modifier.t list * 'a Style.t * 'a Style.arg -> t

let make : type a. ?modifiers:Modifier.t list -> a Style.t -> a Style.arg -> t =
 fun ?(modifiers = []) t a -> C (modifiers, t, a)

let apply_modifier modifier (C (modifiers, s, a)) =
  C (modifier :: modifiers, s, a)

let to_string (C (modifiers, style, arg)) =
  Format.asprintf "%a%s%a"
    (fun fmt () ->
      List.iter
        (fun modifier ->
          Format.pp_print_string fmt (Modifier.to_string modifier);
          Format.pp_print_char fmt ':')
        modifiers)
    () (Style.to_string style)
    (fun fmt () ->
      match arg with
      | Style.Term -> ()
      | _ ->
          Format.pp_print_char fmt '-';
          Format.pp_print_string fmt (Style.arg_to_string arg))
    ()
