let () =
  let w = new Lst_molecules.water in
  let cd = new Lst_molecules.carbon_dioxyde in
  let tnt = new Lst_molecules.tnt in
  let vc = new Lst_molecules.vitamin_c in
  let atp = new Lst_molecules.atp in
  let b = new Lst_molecules.bupropion in
  print_endline w#to_string;
  print_endline cd#to_string;
  print_endline tnt#to_string;
  print_endline vc#to_string;
  print_endline atp#to_string;
  print_endline b#to_string
