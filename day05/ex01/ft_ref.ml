type 'a ft_ref = {mutable contents : 'a;}


let return (value: 'a) : 'a ft_ref =
  {contents = value}


let get (myRef:'a ft_ref) : 'a =
  myRef.contents


let set (myRef:'a ft_ref) (value: 'a) : unit =
  myRef.contents <- value


let bind (myRef: 'a ft_ref) (f: 'a -> 'b ft_ref) : 'b ft_ref =
  (f (get myRef))


let main () =
  let x = return 42
  in
  print_int (get x);
  print_char '\n';

  set x (42 * 2);
  print_int (get x);
  print_char '\n';

  let y = bind x (fun a -> (return (a - 21)))
  in
  print_int (get y);
  print_char '\n'

let () =
  main ()
