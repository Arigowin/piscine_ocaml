let repeat_x n = 
  if n < 0 then
    "Error"
  else 
    let rec loop_str i res =
      if i > 0 then
        loop_str (i - 1) (res ^ "x")
      else
        res
    in loop_str n ""


let main () = 
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5)

let () = main ()