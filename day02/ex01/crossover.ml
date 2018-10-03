
let crossover l1 l2 =
  let rec browse_list acc = function
    | ([], _)                           -> acc
    | (a :: b, [])                      -> browse_list acc (b, l2)
    | (a :: _ as t, c :: d) when a = c  -> browse_list (a :: acc) (t, d)
    | (a :: _ as t, _ :: d)             -> browse_list acc (t, d)
  in
  browse_list [] (l1, l2)


let main () =
  let show_res a =
    Printf.printf "%d, " a
  in
  List.iter show_res (crossover (1 :: 2 :: 3 :: 4 :: 6 :: []) (1 :: 2 :: 5 :: 4 :: [])); (* 4, 2, 1, *)
  print_char '\n';
  List.iter show_res (crossover (1 :: 2 :: 3 :: 4 :: 6 :: []) ([])); (* *nothing* *)
  print_char '\n';
  List.iter show_res (crossover ([]) (1 :: 2 :: 5 :: 4 :: [])); (* *nothing* *)
  print_char '\n';
  List.iter show_res (crossover (9 :: 42 :: 21 :: (-10) :: 999 :: []) (0 :: 997 :: 998 :: 999 :: [])); (* 999, *)
  print_char '\n'

let () = main ()
