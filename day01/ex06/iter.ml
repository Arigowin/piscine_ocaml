let iter f x n  =
  let rec it n acc =
    if n < 0 then
      -1
    else if n = 0 then
      acc
    else
      it (n - 1) (f acc)
  in it n x


let main () = 
  print_int (iter (fun x -> x * x) 2 4 );
  print_char '\n';
  print_int (iter (fun x -> x * 2) 2 4);
  print_char '\n'

let () = main ()
