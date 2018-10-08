let sum i j =
  i +. j


let main () =
  print_float (sum 21.21 21.21);
  print_char '\n';
  print_float (sum 42.0 0.42)

let () = main ()
