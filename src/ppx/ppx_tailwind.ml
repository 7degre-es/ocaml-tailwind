module type ARGS = sig
  val output_path : unit -> string
end

open Ppxlib
open Ast_builder.Default

let map_top_down_from_rule rule =
  object
    inherit Context_free.map_top_down [ rule ]
  end

let str_const_of_str str = Pconst_string (str, !Ast_helper.default_loc, None)

module Klasses = struct
  let klasses = ref []

  let write expr =
    let si =
      [%stri
        let () =
          let open Tailwind.Dsl in
          print_endline (Tailwind.Klass.to_string [%e expr])]
    in
    let arg =
      Format.asprintf {|#use "topfind";; #require "tailwind";; %a;;|}
        Pprintast.structure_item si
    in
    let ((stdout, _, stderr) as full) =
      Unix.open_process_args_full "ocaml" [| "ocaml"; "-e"; arg |] [||]
    in
    let pid = Unix.process_full_pid full in
    let _, status = Unix.waitpid [] pid in
    match status with
    | WEXITED 0 ->
        let klass = input_line stdout in
        klasses := klass :: !klasses;
        Ok ()
    | _ -> Error (In_channel.input_all stderr)

  let expand ~ctxt:_ expr =
    match write expr with
    | Ok () -> expr
    | Error output ->
        let ext =
          Location.error_extensionf ~loc:expr.pexp_loc
            "OCaml command exited uncleanly: %s" output
        in
        pexp_extension ~loc:expr.pexp_loc ext

  let extension =
    Extension.V3.declare "tw" Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expand

  let rule = Context_free.Rule.extension extension
  let mapper = map_top_down_from_rule rule
end

module Num = struct
  let num_ident_of_string ~loc str =
    let str =
      match str with
      | "0" -> Some "zero"
      | "1" -> Some "one"
      | "2" -> Some "two"
      | "3" -> Some "three"
      | "4" -> Some "four"
      | "5" -> Some "five"
      | "6" -> Some "six"
      | "7" -> Some "seven"
      | "8" -> Some "eight"
      | "9" -> Some "nine"
      | "10" -> Some "ten"
      | "11" -> Some "eleven"
      | "12" -> Some "twelve"
      | "14" -> Some "fourteen"
      | "16" -> Some "sixteen"
      | "20" -> Some "twenty"
      | "24" -> Some "twenty_four"
      | "28" -> Some "twenty_eight"
      | "32" -> Some "thirty_two"
      | "36" -> Some "thirty_six"
      | "40" -> Some "fourty"
      | "44" -> Some "fourty_four"
      | "48" -> Some "fourty_eight"
      | "50" -> Some "fifty"
      | "52" -> Some "fifty_two"
      | "56" -> Some "fifty_six"
      | "60" -> Some "sixty"
      | "64" -> Some "sixty_four"
      | "72" -> Some "seventy_two"
      | "80" -> Some "eighty"
      | "96" -> Some "ninety_six"
      | "100" -> Some "one_hundred"
      | "200" -> Some "two_hundred"
      | "300" -> Some "three_hundred"
      | "400" -> Some "four_hundred"
      | "500" -> Some "five_hundred"
      | "600" -> Some "six_hundred"
      | "700" -> Some "seven_hundred"
      | "800" -> Some "eight_hundred"
      | "900" -> Some "nine_hundred"
      | "950" -> Some "nine_hundred_fifty"
      | "0.5" -> Some "point_five"
      | "1.5" -> Some "one_point_five"
      | "2.5" -> Some "two_point_five"
      | "3.5" -> Some "three_point_five"
      | "1_2" -> Some "one_half"
      | "1_3" -> Some "one_third"
      | "2_3" -> Some "two_thirds"
      | "1_4" -> Some "one_fourth"
      | "2_4" -> Some "two_fourths"
      | "3_4" -> Some "three_fourths"
      | "1_5" -> Some "one_fifth"
      | "2_5" -> Some "two_fifths"
      | "3_5" -> Some "three_fifths"
      | "4_5" -> Some "four_fifths"
      | "1_6" -> Some "one_sixth"
      | "2_6" -> Some "two_sixths"
      | "3_6" -> Some "three_sixths"
      | "4_6" -> Some "four_sixths"
      | "5_6" -> Some "five_sixths"
      | _ -> None
    in
    Option.map
      (fun str -> { loc; txt = Ldot (Ldot (Lident "Tailwind", "Num"), str) })
      str

  let expand loc str =
    match num_ident_of_string ~loc str with
    | Some ident -> [%expr Tailwind.Style.Num [%e pexp_ident ~loc ident]]
    | None ->
        let ext = Location.error_extensionf ~loc "Invalid number: %s" str in
        pexp_extension ~loc ext

  let rule = Context_free.Rule.constant Integer 't' expand
  let mapper = map_top_down_from_rule rule
end

let impl ctxt str =
  let ( let* ) (str, errors) f = match errors with [] -> f str | _ -> str in
  let* str = Num.mapper#structure ctxt str in
  let* str = Klasses.mapper#structure ctxt str in
  let strings =
    let loc = !Ast_helper.default_loc in
    let l =
      elist ~loc
        (List.map
           (fun c -> pexp_constant ~loc (str_const_of_str ("tw:" ^ c)))
           !Klasses.klasses)
    in
    let expr = [%expr Sys.opaque_identity [%e l]] in
    [%stri let _ = [%e expr]]
  in
  strings :: str

let () = Driver.V2.register_transformation "tailwind" ~impl
