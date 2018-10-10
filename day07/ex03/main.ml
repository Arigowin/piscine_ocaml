let () = 
  let people = new People.people "Amy Pond" in
  print_endline people#to_string;
  people#talk;
  people#die;
  let doctor = new Doctor.doctor "Unknown" 2000 people in
  print_endline doctor#to_string;
  doctor#talk;
  let newd = doctor#travel_in_time 17 35 in
  print_endline newd#to_string;
  newd#talk;
  newd#use_sonic_screwdriver;
  let dalek = new Dalek.dalek in
  print_endline (dalek#to_string);
  dalek#talk;
  dalek#exterminate people;
  dalek#die;

  print_endline ("----------------- Battle -----------------");

  print_endline (dalek#to_string);
  print_endline (people#to_string);
  dalek#talk;
  dalek#exterminate people;
  dalek#talk;
  print_endline (dalek#to_string);
  print_endline "The Doctor use the sonic screwdriver";
  doctor#use_sonic_screwdriver;
  dalek#die;

  ignore (((new Army.army)#add people)#delete people);
  ignore (((new Army.army)#add (new Dalek.dalek))#add (new Dalek.dalek));
  print_endline "2 army created"