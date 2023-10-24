open Ppxlib
open Ast_builder.Default

module Config = struct
  let use_local = ref false
end

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
      let inc =
        if !Config.use_local then
          let inside_dune = Sys.getenv "INSIDE_DUNE" in
          Printf.sprintf
            {|
#directory "%s/src/main/.tailwind.objs/byte";; 
#load "%s/src/main/tailwind.cma";;|}
            inside_dune inside_dune
        else
          let switch_prefix = Sys.getenv "OPAM_SWITCH_PREFIX" in
          Printf.sprintf
            {|
#directory "%s/lib/tailwind";; 
#load "%s/lib/tailwind/tailwind.cma";;|}
            switch_prefix switch_prefix
      in
      Format.asprintf {|
      %s
      %a;;|} inc Pprintast.structure_item si
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
    let str = "n" ^ String.map (function '.' -> '\'' | c -> c) str in
    { loc; txt = Ldot (Ldot (Lident "Tailwind", "Num"), str) }

  let expand loc str =
    let ident = num_ident_of_string ~loc str in
    [%expr Tailwind.Style.Num [%e pexp_ident ~loc ident]]

  let rule_i = Context_free.Rule.constant Integer 't' expand
  let rule_f = Context_free.Rule.constant Float 't' expand
  let mapper_i = map_top_down_from_rule rule_i
  let mapper_f = map_top_down_from_rule rule_f
end

let impl ctxt str =
  let ( let* ) (str, errors) f = match errors with [] -> f str | _ -> str in
  let* str = Num.mapper_i#structure ctxt str in
  let* str = Num.mapper_f#structure ctxt str in
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

let () =
  Config.use_local := Sys.getenv_opt "PPX_TAILWIND_USE_LOCAL" <> None;
  Driver.V2.register_transformation "tailwind" ~impl
