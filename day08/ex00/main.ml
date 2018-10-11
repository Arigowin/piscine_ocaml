let () =
  let h = new Lst_atom.hydrogen in
  let c = new Lst_atom.carbon in
  let o = new Lst_atom.oxygen in
  let p = new Lst_atom.platinum in
  let a = new Lst_atom.arsenic in
  let z = new Lst_atom.zinc in
  let n = new Lst_atom.nitrogen in
  print_endline h#to_string;
  print_endline c#to_string;
  print_endline o#to_string;
  print_endline p#to_string;
  print_endline a#to_string;
  print_endline z#to_string;
  print_endline n#to_string
