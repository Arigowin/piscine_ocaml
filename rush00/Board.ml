(* module Player =
   struct

   type t = P1 | P2 | None
   type player = { name : string; id : t}

   let toStr = function
   | P1 -> "O "
   | P2 -> "X "
   | None -> "- "

   end *)

type board = Won of Player.t | Open of board list
(* type table = board list list *)

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

let rec print mainBoard =
  match mainBoard with
  | [] -> print_string ("")
  | h1::h2::h3::t -> print_endline(Player.toStr h1 ^ Player.toStr h2 ^ Player.toStr h3 ^ "\n"); print t;
  | _ -> print_string "ERROR"


let splitList board n =
  let rec aux board acc = function
    | 0 -> ( match board with
        | [] -> [acc]
        | _ -> acc :: (aux board [] n))
    | num -> aux ( List.tl board) ((List.hd board)::acc) (num-1)
  in aux board [] n

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

let check_little_won board i size p =
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
  in f a || f b || f c || f d

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

let printLine line =
  let rec loop items = match items with
    | head::tail ->
      begin
        match head with
        | Won (x) -> print_string @@ Player.toStr x; loop tail
        | _ -> ()
      end
    | _ -> print_endline ""
  in
  loop line

let listOf board = match board with
  | Open (x) -> x
  | _ -> []

let printSmallBoard board =
  let boardList = listOf board in
  let rec loop remaining = match remaining with
    | head::tail ->
      begin
        let extracted = extractLineItems 3 0 remaining in
        let line = match (extracted) with | (x, _) -> x in
        let tail = match (extracted) with | (_, t) -> t in
        printLine line;
        loop tail
      end
    | [] -> ()
  in
  loop boardList

let boardLevels board =
  let rec loop acc board = match board with
    | Open (x) -> List.fold_left max 0 (List.map (loop (acc + 1)) x)
    | Won (x) -> acc
  in
  loop 0 board

let firstBoardLine board =
  (* let totalLevels = boardLevels board in *)
  let rec loop level inner = match inner with
    | Won (player) -> (Won player)
    | Open (boards) ->
      begin
        match boards with
        | [] -> Open []
        | innerLst ->
          begin
            let line = (match (extractLineItems 3 0 innerLst) with (x, _) -> x) in
            match line with
            | [] -> Open []
            | x -> Open (List.map (loop (level + 1)) x)
          end
      end
  in
  loop 0 board

let power value p =
  if p = 0 then 1 else
    let rec loop count acc =
      if count = p then acc
      else loop (count + 1) (acc * value)
    in
    loop 1 value


let boardLine row board =
  let totalLevels = boardLevels board in
  (* print_endline @@ string_of_int totalLevels; *)
  let rec loop level row inner = match inner with
    | Won (player) -> (Won player)
    | Open (boards) ->
      begin
        match boards with
        | [] -> Open []
        | innerLst ->
          begin
            let currentRow = 
              if level > 1 then row / (power 3 (3 - level))
              else row mod 3
            in
            let cut = extractLineItems 3 (currentRow * 3) innerLst in
            let lines = match cut with (x, _) -> x in
            (* let otherLines = match cut with (_, x) -> x in *)
            Open (List.map (loop (level - 1) row) lines)
          end
      end
  in
  loop totalLevels row board

let toPrintableBoard board =
  let totalLevels = boardLevels board in
  let totalHeight = power 3 totalLevels in
  (* print_endline @@ string_of_int @@ totalHeight; *)
  let rec loop line =
    if line = totalHeight then []
    else boardLine line board :: loop (line + 1)
  in
  loop 0

let printLine printableBoardLine =
  print_string "| ";
  let rec loop level board =
    begin
      match board with
        | Won (x) -> 
          if level = 1 then print_string @@ String.make (3 + (3 - 1)) (String.get (Player.toStr x) 0) ^ " | "
          else print_string @@ (Player.toStr x)
        | Open (x) -> match x with
          | head::tail -> loop (level + 1) head; List.iter (loop (level + 1)) tail; if level <> 0 then print_string @@ "| ";
          (* | head::tail -> loop head; List.iter loop tail *)
          | [] -> ()
    end;
  in
  loop 0 printableBoardLine;
  print_endline ""

let printBoard printableBoard =
  let seperator = String.make ((3+(3+1)+1)*3 + 1) '-' in
  print_endline @@ seperator;
  let rec loop lines count = match lines with
    | head::tail ->
      begin
        if (count <> 0) && ((count mod 3) = 0) then print_endline @@ seperator;
        printLine head;
        loop tail (count + 1)
      end
    | [] -> ()
  in
  loop printableBoard 0;
  print_endline @@ seperator
