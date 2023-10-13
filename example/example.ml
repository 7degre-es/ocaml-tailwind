open Tyxml.Html
open Tailwind.Dsl
open Tailwind_tyxml

let h1 = h1 ~a:[ classes [ [%tw text x4l]; [%tw font semibold] ] ]
let h2 = h2 ~a:[ classes [ [%tw text x3l]; [%tw font semibold] ] ]
let h3 = h3 ~a:[ classes [ [%tw text x2l]; [%tw font semibold] ] ]

let view =
  html
    (head
       (title (txt "Tailwind example"))
       [ link ~rel:[ `Stylesheet ] ~href:"example.css" () ])
    (body
       [
         h1 [ txt "OCaml x Tailwind" ];
         h2 [ txt "Flexbox" ];
         h3 [ txt "Row" ];
         div
           ~a:[ classes [ [%tw !!flex]; [%tw flex row] ] ]
           [ span [ txt "a" ]; span [ txt "b" ]; span [ txt "c" ] ];
         h3 [ txt "Col" ];
         div
           ~a:[ classes [ [%tw !!flex]; [%tw flex col] ] ]
           [ span [ txt "a" ]; span [ txt "b" ]; span [ txt "c" ] ];
         h2 [ txt "Font styles" ];
         h3 [ txt "Serif" ];
         p ~a:[ classes [ [%tw font mono] ] ] [ txt "Monospace" ];
         p ~a:[ classes [ [%tw font serif] ] ] [ txt "Serif" ];
         h2 [ txt "Modifiers" ];
         h3 [ txt "Hover" ];
         div
           ~a:
             [
               classes [ [%tw !!flex]; [%tw flex row]; [%tw hover' (flex col)] ];
             ]
           [ span [ txt "a" ]; span [ txt "b" ]; span [ txt "c" ] ];
       ])

let () =
  let file = open_out "example.html" in
  let fmt = Format.formatter_of_out_channel file in
  Format.fprintf fmt "%a@." (pp ()) view
