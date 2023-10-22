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
    let ic = Unix.open_process_args_in "ocaml" [| "ocaml"; "-e"; arg |] in
    let pid = Unix.process_in_pid ic in
    let _, status = Unix.waitpid [] pid in
    match status with
    | WEXITED 0 ->
        let klass = input_line ic in
        klasses := klass :: !klasses;
        Ok ()
    | s -> Error s

  let expand ~ctxt:_ expr =
    match write expr with
    | Ok () -> expr
    | Error _ ->
        let ext =
          Location.error_extensionf ~loc:expr.pexp_loc
            "OCaml command exited uncleanly"
        in
        pexp_extension ~loc:expr.pexp_loc ext

  let extension =
    Extension.V3.declare "tw" Extension.Context.expression
      Ast_pattern.(single_expr_payload __)
      expand

  let rule = Context_free.Rule.extension extension
  let mapper = map_top_down_from_rule rule
end

module Color_temperature = struct
  let expand loc str =
    match Tailwind.Color.temperature_of_string str with
    | Some _ ->
        let arg = pexp_constant ~loc (Pconst_string (str, loc, None)) in
        [%expr Option.get (Tailwind.Color.temperature_of_string [%e arg])]
    | None ->
        let ext =
          Location.error_extensionf ~loc "Invalid color temperature: %s" str
        in
        pexp_extension ~loc ext

  let rule = Context_free.Rule.constant Integer 't' expand
  let mapper = map_top_down_from_rule rule
end

let impl ctxt str =
  let str, _ = Color_temperature.mapper#structure ctxt str in
  let str, _ = Klasses.mapper#structure ctxt str in
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
