let () =
  let methane = new Lst_alkanes.methane in
  let ethane = new Lst_alkanes.ethane in
  let octane = new Lst_alkanes.octane in
  print_endline methane#to_string;
  print_endline ethane#to_string;
  print_endline octane#to_string