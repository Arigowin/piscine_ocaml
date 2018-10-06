let rec getInput str =
  let a = print_string(str); read_line ()
  in if a = "" then getInput str else a


let rec get_coords n tab =
  let rec check_string str = match str with
    | "" -> true
    | _ -> (let c = String.get str 0
            in if c < '0' || c > '9' then false else check_string (String.sub str 1 ((String.length str) - 1)))
  and convert_coord yy xx n =
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
    in if not (check_string y) || not (check_string x)
    then redo ("Incorrect format")
    else (
      let x = int_of_string(x) and y = int_of_string(y)
      in if x > 0 && x <= (n*n) && y > 0 && y <= (n*n) && (Board.check_pos tab (convert_coord y x n))
      then (convert_coord y x n)
      else redo ("Illegal move")))
  else redo ("Incorrect format")


let rec game tab size p1 p2 =
  Board.printBoard (Board.toPrintableBoard tab);
  print_endline("[ " ^ Player.toStr p1.Player.id ^ "] " ^ p1.Player.name ^ "'s turn to play.");
  let (c, i) = get_coords size tab
  in let newtab = Board.play tab (c, i) p1.Player.id
  in let scored = Board.check_won newtab size c i p1.Player.id
  in let newtab = if scored then Board.mark_won newtab c p1.Player.id else newtab
  in let won = Board.check_little_won newtab c size p1.Player.id
  in if won
  then (
    Board.printBoard (Board.toPrintableBoard newtab);
    print_endline(p1.Player.name ^ " WON THE GAME."))
  else game newtab size p2 p1


let main () =
  let p1 = { Player.name = getInput("Name of player 1 : "); Player.id = Player.P1 }
  in let rec get_p2_name () =
       let name = getInput("Name of player 2 : ")
       in if (String.equal name p1.Player.name)
       then (print_endline("You can't pick the same name! Try again. "); get_p2_name ())
       else name
  in let p2 = { Player.name = (get_p2_name ()); Player.id = Player.P2 }
  and n = int_of_string(getInput("How many cells? "))
  in let tab = (Board.table n Board.square)
  in game tab n p1 p2


let () =
  main ()
