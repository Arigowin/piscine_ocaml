type board = Won of Player.t | Open of board list


let square num =
  let rec loop = function
    | 0 -> []
    | n -> (Won Player.None) :: loop(n-1)
  in Open (loop (num*num))


let table num f =
  let rec loop = function
    | 0 -> []
    | n -> (f num) :: loop(n-1)
  in Open (loop (num*num))


let megatable num =
  let rec loop = function
    | 0 -> []
    | n -> (table num square) :: loop(n-1)
  in Open (loop (num*num))


let listOf board = match board with
  | Open (x) -> x
  | _ -> []


let rec mark_won board n p = match n with
  | 0 ->( match board with
      | Open (h::t) -> Open (Won p :: t)
      | _ -> board)
  | _ -> match board with
    | Open (h::t) -> Open ( h :: (match (mark_won (Open t) (n-1) p) with | Open (x) -> x | _ -> t ))
    | _ -> board


let play board (c, i) p =
  let rec aux board n = match n with
    | 0 -> (match board with
        | Open (h::t) -> Open ((mark_won h i p) :: t)
        | _ -> board)
    | _ -> match board with
      | Open (h::t) -> Open ( h :: (match (aux (Open t) (n-1)) with | Open (x) -> x | _ -> t ))
      | _ -> board
  in aux board c


let convert_coord yy xx n =
  let x = (xx - 1) and y = (yy - 1)
  in let c = ((y / n) * n) + (x / n)
  and i = ((y mod n) * n) + (x mod n)
  in (c , i)


(* returns true when the cell is clear, false when the move is invalid *)
let check_pos board (c, i) =
  let rec aux board n = match n with
    | 0 -> (match board with
        | Open (h::t) -> (match h with
            | Won x -> if x = Player.None then true else false
            | _ -> aux h i)
        | _ -> false)
    | _ -> match board with
      | Open (h::t) -> aux (Open t) (n-1)
      | _ -> false
  in aux board c


let is_player = function | Won x -> true | _ -> false

let rec count_cells p board =
  let aux h = if not (is_player h) || h = (Won p) then 1 else 0
  in match board with
  | h::t -> aux h + count_cells p t
  | [] -> 0

let count_empty_cells = count_cells Player.None

let check_little_won board i size p =
  if count_empty_cells (listOf board) = 0 then true else (
    let rec aux num start incr = match num with
      | 0 -> []
      | _ -> start :: aux (num-1) (start + incr) incr
    in let a = aux size (i mod size) size
    and b = aux size (i / size * size) 1
    and c = aux size 0 (size + 1)
    and d = aux size (size-1) (size-1)
    and get_list  = function | (Open x) -> x | _ -> []
    in let check_match n = match (List.nth (get_list board) n) with
        | Won x -> Won x = Won p
        | Open x -> false
    in let f x = List.for_all (fun n -> check_match n) x
    in f a || f b || f c || f d )


let rec check_won board size c i p  = match c with
  | 0 ->( match board with
      | Open (h::t) -> check_little_won h i size p
      | _ -> false)
  | _ -> match board with
    | Open (h::t) -> check_won (Open t) size (c-1) i p
    | _ -> false


(* ---------------------------------------------------------------------------------------- *)

let extractLineItems count offset list =
  let rec loop items count offset acc = match count with
    | 0 -> (acc, items)
    | n ->
      if offset = 0 then loop (List.tl items) (n - 1) 0 (acc @ [List.hd items])
      else loop (List.tl items) n (offset - 1) (acc)
  in
  loop list count offset []


let boardLevels board =
  let rec loop acc board = match board with
    | Open (x) -> List.fold_left max 0 (List.map (loop (acc + 1)) x)
    | Won (x) -> acc
  in
  loop 0 board


let power value p =
  if p = 0 then 1 else
    let rec loop count acc =
      if count = p then acc
      else loop (count + 1) (acc * value)
    in
    loop 1 value


let boardLine row board size =
  let totalLevels = boardLevels board in
  let rec loop level row inner = match inner with
    | Won (player) -> (Won player)
    | Open (boards) ->
      begin
        match boards with
        | [] -> Open []
        | innerLst ->
          begin
            let currentRow =
              row / (power size (level - 1))
            in
            let cut = extractLineItems size (currentRow * size) innerLst in
            let lines = match cut with (x, _) -> x in
            Open (List.map (loop (level - 1) (row - (currentRow * (power size (level - 1))))) lines)
          end
      end
  in
  loop totalLevels row board


let toPrintableBoard board size =
  let totalLevels = boardLevels board in
  let totalHeight = power size totalLevels in
  let rec loop line =
    if line = totalHeight then []
    else boardLine line board size :: loop (line + 1)
  in
  loop 0


let printLine printableBoardLine size =
  print_string "| ";
  let rec loop level board =
    begin
      match board with
      | Won (x) ->
        if level = 1 then print_string @@ String.make (size + (size - 1)) (String.get (Player.toStr x) 0) ^ " | "
        else print_string @@ (Player.toStr x)
      | Open (x) -> match x with
        | head::tail -> loop (level + 1) head; List.iter (loop (level + 1)) tail; if level <> 0 then print_string @@ "| ";
        | [] -> ()
    end;
  in
  loop 0 printableBoardLine;
  print_endline ""


let seperator level size =
  let rec sizeLoop localLevel localSize = match localSize with
    | 0 -> ""
    | _ -> (levelLoop (localLevel - 1)) ^ (sizeLoop localLevel (localSize - 1))
  and levelLoop localLevel = match localLevel with
    | 0 -> "--"
    | _ -> "-" ^ (sizeLoop localLevel size) ^ (if localLevel <> level then "-" else "")
  in let res = levelLoop level in
  res

let rec applyNTimes n f =
  if n = 0 then ()
  else (f(); applyNTimes (n - 1) f)

let printBoard printableBoard levels size =
  let seperator = seperator levels size in
  let expectedSize = (size * size) in
  print_endline @@ seperator;
  let rec loop lines count = match lines with
    | head::tail ->
      begin
        if (count <> 0) && ((count mod size) = 0) then print_endline @@ seperator;
        (if expectedSize <> (List.length printableBoard) then
           (applyNTimes size (fun () -> (printLine head size));
            if count <> (size - 1) then print_endline @@ seperator)
         else printLine head size);
        loop tail (count + 1)
      end
    | [] -> ()
  in
  loop printableBoard 0;
  print_endline @@ seperator
