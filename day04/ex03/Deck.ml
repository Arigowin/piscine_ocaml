module Card =
struct

  module Color =
  struct

    type t = Spade | Heart | Diamond | Club

    let all = [Spade ; Heart ; Diamond ; Club]

    let toString = function
      | Spade -> "S"
      | Heart -> "H"
      | Diamond -> "D"
      | Club -> "C"

    let toStringVerbose = function
      | Spade -> "Spade"
      | Heart -> "Heart"
      | Diamond -> "Diamond"
      | Club -> "Club"

  end


  module Value =
  struct

    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [T2 ; T3 ; T4 ; T5 ; T6 ; T7 ; T8 ; T9 ; T10 ; Jack ; Queen ; King ; As]

    let toInt = function
      | T2      -> 1
      | T3      -> 2
      | T4      -> 3
      | T5      -> 4
      | T6      -> 5
      | T7      -> 6
      | T8      -> 7
      | T9      -> 8
      | T10     -> 9
      | Jack    -> 10
      | Queen   -> 11
      | King    -> 12
      | As      -> 13

    let toString = function
      | T2      -> "2"
      | T3      -> "3"
      | T4      -> "4"
      | T5      -> "5"
      | T6      -> "6"
      | T7      -> "7"
      | T8      -> "8"
      | T9      -> "9"
      | T10     -> "10"
      | Jack    -> "J"
      | Queen   -> "Q"
      | King    -> "K"
      | As      -> "A"


    let toStringVerbose = function
      | T2      -> "2"
      | T3      -> "3"
      | T4      -> "4"
      | T5      -> "5"
      | T6      -> "6"
      | T7      -> "7"
      | T8      -> "8"
      | T9      -> "9"
      | T10     -> "10"
      | Jack    -> "Jack"
      | Queen   -> "Queen"
      | King    -> "King"
      | As      -> "As"

    let next = function
      | T2      -> T3
      | T3      -> T4
      | T4      -> T5
      | T5      -> T6
      | T6      -> T7
      | T7      -> T8
      | T8      -> T9
      | T9      -> T10
      | T10     -> Jack
      | Jack    -> Queen
      | Queen   -> King
      | King    -> As
      | As      -> invalid_arg "No value after As"

    let previous = function
      | T2      -> invalid_arg "No value before T2"
      | T3      -> T2
      | T4      -> T3
      | T5      -> T4
      | T6      -> T5
      | T7      -> T6
      | T8      -> T7
      | T9      -> T8
      | T10     -> T9
      | Jack    -> T10
      | Queen   -> Jack
      | King    -> Queen
      | As      -> King

  end


  type t = Value.t * Color.t

  let newCard value color =
    (value, color)

  let allSpades = 
    List.map (fun x -> newCard x Color.Spade) Value.all

  let allHearts = 
    List.map (fun x -> newCard x Color.Heart) Value.all

  let allDiamonds = 
    List.map (fun x -> newCard x Color.Diamond) Value.all

  let allClubs = 
    List.map (fun x -> newCard x Color.Club) Value.all

  let all = 
    List.concat [allSpades ; allHearts ; allDiamonds ; allClubs]

  let getValue (v, _) =
    v

  let getColor (_, c) =
    c

  let toString (v, c) =
    (Value.toString v) ^ (Color.toString c)

  let toStringVerbose (v, c) =
    Printf.sprintf "Card(%s,%s)" (Value.toStringVerbose v) (Color.toStringVerbose c)

  let compare (v1, _) (v2, _) =
    (Value.toInt v1) - (Value.toInt v2)

  let max c1 c2 =
    if (compare c1 c2) < 0 then c2
    else                        c1

  let min c1 c2 =
    if (compare c1 c2) > 0 then c2
    else                        c1

  let best = function
    | []     -> invalid_arg "List is empty"
    | a :: b -> List.fold_left max a b

  let isOf ((_, c) : t) (c_ref : Color.t) =
    c = c_ref

  let isSpade = function c ->
    isOf c Color.Spade

  let isHeart = function c ->
    isOf c Color.Heart

  let isDiamond = function c ->
    isOf c Color.Diamond

  let isClub = function c ->
    isOf c Color.Club

end


type t = Card.t list

let newDeck () =
  let l = 
    List.map (fun c -> (Random.bits (), c)) Card.all in
  let sorted_l = 
    List.sort compare l
  in List.map snd sorted_l

let toStringList = 
  List.map Card.toString

let toStringListVerbose = 
  List.map Card.toStringVerbose

let drawCard = function
  | []      -> raise (Failure ("Empty Deck"))
  | a :: b  -> (a, b)