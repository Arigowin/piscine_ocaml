let create_array_of_jokes () =
  let jokes = Array.make 5 "";
  in
  jokes.(0) <- "A drummer and a bass player jump off a skyscraper – Boom Boom.";
  jokes.(1) <- "You’ll never believe whom I saw yesterday! Everybody I laid my eyes on!";
  jokes.(2) <- "What is transparent and smells like worms? - A bird's fart :-)";
  jokes.(3) <- "One twin to the other: \"You are ugly.\"";
  jokes.(4) <- "Girl: So, how many times a day do you shave?\nMan: Well, about 15-20 times every day.\nGirl: My god, are you some kind of crazy?\nMan: No, I’m a barber.";
  jokes


let ft_jokes () =
  Random.self_init();
  let jokes = create_array_of_jokes ()
  in
  print_endline jokes.(Random.int 5)


let main () =
  ft_jokes ()


let () = main ()
