let () =
  let w = new Lst_molecule.water in
  let cd = new Lst_molecule.carbon_dioxyde in
  let tnt = new Lst_molecule.tnt in
  let vc = new Lst_molecule.vitamin_c in
  let atp = new Lst_molecule.atp in
  let b = new Lst_molecule.bupropion in
  print_endline w#to_string;
  print_endline cd#to_string;
  print_endline tnt#to_string;
  print_endline vc#to_string;
  print_endline atp#to_string;
  print_endline b#to_string
