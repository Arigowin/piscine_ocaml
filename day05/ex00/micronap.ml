let my_sleep s = Unix.sleep s

let main av =
  if Array.length av = 2 then
    my_sleep (int_of_string av.(1))

let () =
  main Sys.argv

(* for compile use this *)
(* ocamlfind ocamlc -linkpkg -package unix micronap.ml *)
