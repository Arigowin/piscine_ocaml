let rec getInput str =
  let a = print_string(str); read_line ()
  in if a = "" then getInput str else a


let rec string_is_digits str = match str with
  | "" -> true
  | _ -> (let c = String.get str 0
          in if c < '0' || c > '9' then false else string_is_digits (String.sub str 1 ((String.length str) - 1)))


let rec get_coords n tab =
  let convert_coord yy xx n =
    let x = (xx - 1) and y = (yy - 1)
    in let c = ((y / n) * n) + (x / n)
    and i = ((y mod n) * n) + (x mod n)
    in (c , i)
  and redo str = print_endline(str); get_coords n tab
  and input = getInput("")
  in let l = List.filter (fun x -> x <> "") (String.split_on_char ' ' input)
  in if List.length l = 2
  then (
    let y = (List.hd l) and x = (List.nth l 1)
    in if not (string_is_digits y) || not (string_is_digits x)
    then redo ("Incorrect format")
    else (
      let x = int_of_string(x) and y = int_of_string(y)
      in if x > 0 && x <= (n*n) && y > 0 && y <= (n*n) && (Board.check_pos tab (convert_coord y x n))
      then (convert_coord y x n)
      else redo ("Illegal move")))
  else redo ("Incorrect format")


let wants_to_play_again () = print_string ("Do you want to play another game? (The loser will start) N/y ");
  if String.equal (String.trim (read_line ())) "y" then true else false

let rec game tab size levels p1 p2 =
  Board.printBoard (Board.toPrintableBoard tab size) levels size;
  print_endline("\n[ " ^ Player.toStr p1.Player.id ^ "] " ^ p1.Player.name ^ "'s turn to play.");

  let (c, i) = if String.equal p1.Player.name "AI"
    then Ai.get_ai_coords tab
    else get_coords size tab

  in let newtab = Board.play tab (c, i) p1.Player.id
  in let scored = Board.check_won newtab size c i p1.Player.id
  in let newtab = if scored then Board.mark_won newtab c p1.Player.id else newtab
  in let won = Board.check_little_won newtab c size p1.Player.id
  in if won
  then (
    Board.printBoard (Board.toPrintableBoard newtab size) levels size;
    print_endline(p1.Player.name ^ " WON THE GAME.");
    if wants_to_play_again () then game (Board.table size Board.square) size levels p2 p1 )
  else game newtab size levels p2 p1


let print_intro() =
  print_endline("Welcome to our game.\nYou can only play against an AI if you choose a size of 3x3")


let main () =
  let rec get_size () =
    let s = String.trim (getInput ("How many cells? ")) in
    if string_is_digits s && int_of_string(s) > 1 && int_of_string(s) <= 6
    then int_of_string(s)
    else (print_endline("The board size must be a number between 2 and 6."); get_size ())
  and get_p1_name ai =
    let name = getInput("Name of player 1 : ")
    in if ai && (String.equal (String.uppercase_ascii name) "AI")
    then (print_endline("You can't be called AI!. You're not that smart "); get_p1_name ai)
    else name
  and get_ai () = print_string ("Do you want to play against an AI? N/y ");
    if String.equal (String.trim (read_line ())) "y" then true else false
  in let rec get_p2_name p1 =
       let name = getInput("Name of player 2 : ")
       in if (String.equal name p1.Player.name)
       then (print_endline("You can't pick the same name! Try again. "); get_p2_name p1)
       else name
  in print_intro();
  let n = get_size()
  in let ai = if n = 3 then get_ai () else false
  in let p1 = { Player.name = get_p1_name ai; Player.id = Player.P1 }
  in let p2 = if ai then { Player.name = "AI"; Player.id = Player.P2 }
       else { Player.name = (get_p2_name p1); Player.id = Player.P2 }
  in let tab = (Board.table n Board.square)
  in let levels = (Board.boardLevels tab)
  in game tab n levels p1 p2


let () =
  main ()
