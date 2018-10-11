let () =
  let methane = new Lst_alkanes.methane in
  let ethane = new Lst_alkanes.ethane in
  let octane = new Lst_alkanes.octane in
  let f1 = new Alkane.alkane 13 in
  let f2 = new Alkane.alkane 0 in
  print_endline methane#to_string;
  print_endline ethane#to_string;
  print_endline octane#to_string;
  print_endline f1#to_string;
  print_endline f2#to_string