module type ARGS = sig
  val output_path : unit -> string
end

module Make (M : ARGS) = struct
  open Ppxlib
  open Ast_builder.Default

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
        let oc =
          open_out_gen
            [ Open_append; Open_creat ]
            0o666
            (Printf.sprintf "%s/tw_classes" (M.output_path ()))
        in
        Printf.fprintf oc "%s\n" klass;
        close_out oc;
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
  let () = Driver.register_transformation ~rules:[ rule ] "tailwind"
end
