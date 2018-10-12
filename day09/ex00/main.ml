let () = 
  print_int (Watchtower.Watchtower.add 5 10); (* 3 *)
  print_char '\n';
  print_int (Watchtower.Watchtower.add 5 12); (* 5 *)
  print_char '\n';
  print_int (Watchtower.Watchtower.add 5 3); (* 8 *)
  print_char '\n';
  print_int (Watchtower.Watchtower.sub 1 3); (* 10 *)
  print_char '\n';
  print_int (Watchtower.Watchtower.sub 5 3); (* 2 *)
  print_char '\n';
  print_int Watchtower.Watchtower.zero; (* 12 *)
  print_char '\n'