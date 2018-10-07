let make_lists () =
  ([0; 1; 2], [3; 4; 5], [6; 7; 8], [0; 3; 6], [1; 4; 7], [2; 5; 8], [0; 4; 8], [2;4;6])


let check_condition ff =
  let (a, b, c, d, e, f, g, h) = make_lists ()
  in ff a || ff b || ff c || ff d || ff e || ff f || ff g || ff h


let one_combo_is_clear board p =
  let check n = (List.nth board n = Board.Won Player.None) || (List.nth board n = Board.Won p)
  in check_condition (fun x -> List.for_all (fun n -> check n) x)

let combination_score n p board =
  if List.nth board n = Board.Won p then 2
  else if List.nth board n = Board.Won Player.None then 1 else 5


let is_about_to_win board p score =
  check_condition (fun x -> ( List.fold_left (fun acc n -> acc + (combination_score n p board)) 0 x) = score)


let rec player_can_score p board score = match board with
  | Board.Open (h::t) ->
    ( match h with
      | Won x -> player_can_score p (Board.Open t) score
      | Open x -> if (is_about_to_win (Board.listOf h) p score) then true else player_can_score p (Board.Open t) score )
  | _ -> false


let stop_enemy_or_win small_b p score : int =
  let (a, b, c, d, e, f, g, h) = make_lists ()
  in let lst = [a; b; c; d; e; f; g; h]
  in let correct_combination = List.find (fun x -> (List.fold_left (fun acc n -> acc + (combination_score n p (Board.listOf small_b))) 0 x) = score) lst
  in let position = List.find (fun x -> List.nth (Board.listOf small_b) x = Board.Won Player.None) correct_combination
  in position


let rec find_where_to_block_or_win board p c score : (int * int ) = match board with
  | Board.Open (h::t) -> (
      match h with
      | Won x -> find_where_to_block_or_win (Board.Open t) p (c+1) score
      | Open x -> (if (is_about_to_win (Board.listOf h) p score)
                   then (c, (stop_enemy_or_win h p score))
                   else find_where_to_block_or_win (Board.Open t) p (c+1) score))
  | _ -> (0, 0)


let place_x board : int =
  if List.nth (Board.listOf board) 4 = Board.Won Player.None then 4
  else if List.nth (Board.listOf board) 8 = Board.Won Player.None then 8
  else if List.nth (Board.listOf board) 2 = Board.Won Player.None then 2
  else if List.nth (Board.listOf board) 0 = Board.Won Player.None then 0
  else if List.nth (Board.listOf board) 6 = Board.Won Player.None then 6
  else if List.nth (Board.listOf board) 1 = Board.Won Player.None then 1
  else if List.nth (Board.listOf board) 3 = Board.Won Player.None then 3
  else if List.nth (Board.listOf board) 5 = Board.Won Player.None then 5
  else 7


let play_somewhere board c f : (int * int) =
  let cell x = List.nth (Board.listOf board) x in
  if not (Board.is_player (cell 4)) && (f (cell 4)) then (4, (place_x (cell 4)))
  else if not (Board.is_player (cell 8)) && (f (cell 8)) then (8, (place_x (cell 8)))
  else if not (Board.is_player (cell 2)) && (f (cell 2)) then (2, (place_x (cell 2)))
  else if not (Board.is_player (cell 0)) && (f (cell 0)) then (0, (place_x (cell 0)))
  else if not (Board.is_player (cell 6)) && (f (cell 6)) then (6, (place_x (cell 6)))
  else if not (Board.is_player (cell 1)) && (f (cell 1)) then (1, (place_x (cell 1)))
  else if not (Board.is_player (cell 3)) && (f (cell 3)) then (3, (place_x (cell 3)))
  else if not (Board.is_player (cell 5)) && (f (cell 5)) then (5, (place_x (cell 5)))
  else if not (Board.is_player (cell 7)) && (f (cell 7)) then (7, (place_x (cell 7)))
  else (0, 0)

let enemyOf p = if p = Player.P1 then Player.P2 else Player.P1


let has_advantage p small_b =
  let num = if p = (enemyOf p) then 0 else 1 in
  let your_count = Board.count_cells p (Board.listOf small_b)
  and enemys_count = Board.count_cells (enemyOf p) (Board.listOf small_b)
  in if (Board.is_player small_b) && small_b = Won p then true
  else (
    if your_count > enemys_count then true
    else if enemys_count < your_count then false
    else ( if (Board.count_empty_cells (Board.listOf small_b)) mod 2 = num
           then true else false
         ))

let big_combination_score n p board =
  let b = (List.nth board n) in
  if Board.count_empty_cells (Board.listOf b) = 9 then 1
  else if has_advantage p b then 2
  else 5

let is_about_to_win_in_big_table board p score =
  check_condition (fun x -> ( List.fold_left (fun acc n -> acc + (big_combination_score n p board)) 0 x) = score)

let rec big_table_strategy board p score : int =
  let (a, b, c, d, e, f, g, h) = make_lists ()
  in let lst = [a; b; c; d; e; f; g; h]
  in let correct_combination = List.find (fun x -> (List.fold_left (fun acc n -> acc + (big_combination_score n p board)) 0 x) = score) lst
  in let position = List.find (fun x -> Board.count_empty_cells (Board.listOf (List.nth board x)) = 9) correct_combination
  in position

let get_ai_coords board =
  let list_b = (Board.listOf board) in
  let has_advantage x = Board.count_empty_cells (Board.listOf x) mod 2 = 1
  in if (player_can_score Player.P1 board 5) then find_where_to_block_or_win board Player.P1 0 5
  else if (is_about_to_win_in_big_table list_b Player.P2 5) then (
    let c = (big_table_strategy list_b Player.P2 5) in
    (c, place_x (List.nth list_b c)))
  else if (is_about_to_win_in_big_table list_b Player.P1 5) then (
    let c = (big_table_strategy list_b Player.P1 5) in
    (c, place_x (List.nth list_b c)))
  else if (is_about_to_win_in_big_table list_b Player.P2 4) then (
    let c = (big_table_strategy list_b Player.P2 4) in
    (c, place_x (List.nth list_b c)))
  else if (player_can_score Player.P2 board 5) then find_where_to_block_or_win board Player.P2 0 5
  else if (player_can_score Player.P2 board 4 ) then find_where_to_block_or_win board Player.P2 0 4
  else if (play_somewhere board 0 has_advantage) <> (0, 0) then play_somewhere board 0 has_advantage
  else play_somewhere board 0 (fun x -> true)
