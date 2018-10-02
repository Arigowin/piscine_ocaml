let fibonacci n =
  let rec fib n =
    if n > 1 then
      fib (n - 2) + fib (n - 1)
    else if n = 1 then
      1
    else if n = 0 then
      0
    else
      -1
  in fib n

let main () = 
  print_int (fibonacci (-42));
  print_char '\n';
  print_int (fibonacci 1);
  print_char '\n';
  print_int (fibonacci 3);
  print_char '\n';
  print_int (fibonacci 6);
  print_char '\n'

let () = main ()