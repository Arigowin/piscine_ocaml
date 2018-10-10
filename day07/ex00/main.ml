let () = 
  let p = new People.people "The Master" in
  print_endline p#to_string;
  p#talk;
  p#die