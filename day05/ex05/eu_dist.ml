let eu_dist (tab1: float array) (tab2: float array) : float =
  let tab3 = Array.make (Array.length tab1) 0.0
  in
  for i = 0 to ((Array.length tab1) - 1) do
    tab3.(i) <- (tab1.(i) -. tab2.(i)) *. (tab1.(i) -. tab2.(i))
  done;
  let res = ref 0.0
  in
  for i = 0 to ((Array.length tab3) - 1) do
    res := res.contents +. tab3.(i)
  done;
  sqrt res.contents


let main () =
  let tab1 = Array.make 2 0.0
  in let tab2 = Array.make 2 0.0
  in
  tab1.(0) <- (-0.05889);
  tab1.(1) <- (-0.85243);
  tab2.(0) <- 0.92806;
  tab2.(1) <- 0.36174;
  print_float (eu_dist tab1 tab2);
  print_char '\n'


let () = main ();
