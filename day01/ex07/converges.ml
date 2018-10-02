let rec converges f x n  =
  if n < 0 then
    false
  else if n = 0 then
    true
  else if n = 1 then
    (f x) = x
  else
    (converges f (f x) (n - 1))

let main () = 
  let test a = 
    if a then 
      print_endline "true" 
    else 
      print_endline "false" 
  in
  test (converges (( * ) 2) 2 5);
  test (converges (fun x -> x / 2) 2 3);
  test (converges (fun x -> x / 2) 2 2)

let () = main ()
