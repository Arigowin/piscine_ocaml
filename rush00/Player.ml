type t = P1 | P2 | None
type player = { name : string; id : t}

let toStr = function
  | P1 -> "O "
  | P2 -> "X "
  | None -> "- "
