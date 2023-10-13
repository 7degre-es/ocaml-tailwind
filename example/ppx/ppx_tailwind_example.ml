include Ppx_tailwind.Make (struct
  let output_path () =
    Printf.sprintf "%s/example" (Sys.getenv "DUNE_SOURCEROOT")
end)
