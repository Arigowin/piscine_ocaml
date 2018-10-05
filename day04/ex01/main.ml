let main () =
  List.iter print_endline (List.map Value.toString Value.all);
  List.iter print_endline (List.map Value.toStringVerbose Value.all);
  print_endline (Value.toString (Value.next Value.T2));
  print_endline (Value.toString (Value.next Value.King));
  print_endline (try (Value.toString (Value.next Value.As))
                 with Invalid_argument(_) -> "No value after As");
  print_endline (try (Value.toString (Value.previous Value.T2))
                 with Invalid_argument(_) -> "No value before T2");
  print_endline (Value.toString (Value.previous Value.King));
  print_endline (Value.toString (Value.previous Value.As))

let () = main ()