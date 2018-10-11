let () =
  let methane = new Lst_alkanes.methane in
  let ethane = new Lst_alkanes.ethane in
  let octane = new Lst_alkanes.octane in
  let i13 = new Lst_alkanes.invalid13 in
  let i0 = new Lst_alkanes.invalid0 in
  print_endline methane#to_string;
  print_endline ethane#to_string;
  print_endline octane#to_string;
  print_endline i13#to_string;
  print_endline i0#to_string