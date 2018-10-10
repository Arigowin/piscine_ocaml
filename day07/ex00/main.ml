let () = 
  let p = new People.people "Amy Pond" in
  print_endline p#to_string;
  p#talk;
  p#die