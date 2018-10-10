let () = 
  let p = new People.people "Amy Pond" in
  print_endline p#to_string;
  p#talk;
  p#die;
  let d = new Doctor.doctor "Unknown" 2000 p in
  print_endline d#to_string;
  d#talk;
  let newd = d#travel_in_time 17 35 in
  print_endline newd#to_string;
  newd#talk;
  newd#use_sonic_screwdriver;
  let dalek = new Dalek.dalek in
  print_endline (dalek#to_string);
  dalek#talk;
  dalek#exterminate p;
  dalek#die
