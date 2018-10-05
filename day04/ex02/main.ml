let main () =
  List.iter print_endline (List.map Card.toStringVerbose Card.allSpades);
  List.iter print_endline (List.map Card.toString Card.allHearts);
  List.iter print_endline (List.map Card.toString Card.allDiamonds);
  List.iter print_endline (List.map Card.toString Card.allClubs);
  print_endline "";
  List.iter print_endline (List.map Card.toString Card.all);
  print_endline "";
  print_endline (Card.toString (Card.best Card.all))


let () = main ()
