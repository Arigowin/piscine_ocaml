let encode l =
  let rec list_to_tuple acc = function
    | []      -> acc
    | a :: b  -> list_to_tuple (a :: acc) b
  in let rec browse_list count acc = function
      | []                            -> []
      | [elem]                        -> (count + 1, elem) :: acc
      | a :: (b :: _ as t) when a = b -> browse_list (count + 1) acc t
      | a :: t                        -> browse_list 0 ((count + 1, a) :: acc) t
  in list_to_tuple [] (browse_list 0 [] l) 


let main () =
  let show_res (a, b) = 
    Printf.printf "(%d, %d), " a b 
  in
  List.iter show_res (encode (7 :: 7 :: 7 :: 3 :: 2 :: (-1) :: (-1) :: (-1) :: (-1):: 0 :: 0 :: 4 :: [])); (* res -> (3, 7), (1, 3), (1, 2), (4, -1), (2, 0), (1, 4), *)
  print_char '\n';
  List.iter show_res (encode ([])); (* res -> *nothing* *)
  print_char '\n';
  List.iter show_res (encode (0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: 0 :: [])) (* (12, 0), *)

let () = main ()