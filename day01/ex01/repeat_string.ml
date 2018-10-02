let repeat_string ?(str="x") n = 
  if n < 0 then
    "Error"
  else 
    let rec loop_str i res =
      if i > 0 then
        loop_str (i - 1) (res ^ str)
      else
        res
    in loop_str n ""


let main () = 
  print_endline (repeat_string (-1));
  print_endline (repeat_string 0);
  print_endline (repeat_string ~str:"Toto" 1);
  print_endline (repeat_string 2);
  print_endline (repeat_string ~str:"a" 5);
  print_endline (repeat_string ~str:"what" 3)

let () = main ()