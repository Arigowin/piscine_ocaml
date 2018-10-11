let () =
  let h = new Hydrogen.hydrogen in
  let c = new Carbon.carbon in
  let o = new Oxygen.oxygen in
  let p = new Platinum.platinum in
  let a = new Arsenic.arsenic in
  let z = new Zinc.zinc in
  let n = new Nitrogen.nitrogen in
  print_endline h#to_string;
  print_endline c#to_string;
  print_endline o#to_string;
  print_endline p#to_string;
  print_endline a#to_string;
  print_endline z#to_string;
  print_endline n#to_string
