module type App =
sig
  type project = string * string * int
  val zero : project
  val combine : project -> project -> project
  val fail : project -> project
  val success : project -> project
end


module App =
struct
  (* type, status, grade*)
  type project = string * string * int

  let zero : project = ("", "", 0)

  let combine (p1:project) (p2:project) : project =
    match p1 with
    | a, b, c  ->
      (match p2 with
       | d, e, f ->
         let average = (c + f) / 2
         in (a ^ d, (if average > 80 then "succedd" else "failed"), average)
      )

  let fail (p:project): project =
    match p with
    | a, b, c  -> (a, "failed", 0)

  let success (p:project): project =
    match p with
    | a, b, c  -> (a, "succed", 80)
end

let print_proj (p:App.project) : unit =
  match p with
  | a, b, c  -> print_endline ("Type: " ^ a ^ " status: " ^ b ^ " grad: " ^ (string_of_int c))
