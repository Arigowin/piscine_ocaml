module type FIXED =
sig
  type t
  val of_float : float -> t
  val of_int : int -> t
  val to_float : t -> float
  val to_int : t -> int
  val to_string : t -> string
  val zero : t
  val one : t
  val succ : t -> t
  val pred : t -> t
  val min : t -> t -> t
  val max : t -> t -> t
  val gth : t -> t -> bool
  val lth : t -> t -> bool
  val gte : t -> t -> bool
  val lte : t -> t -> bool
  val eqp : t -> t -> bool (** physical equality *)
  val eqs : t -> t -> bool (** structural equality *)
  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t
  val foreach : t -> t -> (t -> unit) -> unit
end

(****************************************************************************************)

module type FRACTIONNAL_BITS =
sig
  val bits : int
end

module type MAKE =
  functor (Fb: FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
  functor (Fb: FRACTIONNAL_BITS) ->
  struct
    type t = int
    let of_float (x:float) = 
      let round v = 
        if ((ceil v) -. v) < (v -. (floor v)) then ceil v else floor v
      in int_of_float (round (x *. (float_of_int (1 lsl Fb.bits))))
    let of_int x = x lsl Fb.bits
    let to_float x = (float_of_int x) /. (float_of_int (1 lsl Fb.bits))
    let to_int x = x lsr Fb.bits
    let to_string x = string_of_float (to_float x)
    let zero = 0
    let one = of_int 1
    let succ = ( + ) 1
    let pred x = x - 1
    let min x1 x2 = if x1 <= x2 then x1 else x2
    let max x1 x2 = if x1 >= x2 then x1 else x2
    let gth = ( > )
    let lth = ( < )
    let gte = ( >= )
    let lte = ( <= )
    let eqp = ( == )
    let eqs = ( = )
    let add = ( + )
    let sub = ( - )
    let mul x1 x2 = of_float ((to_float x1) *. (to_float x2))
    let div x1 x2 = of_float ((to_float x1) /. (to_float x2))
    let rec foreach x1 x2 f = if lte x1 x2 then (f x1; foreach (succ x1) x2 f)
  end


(****************************************************************************************)

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)

let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
  print_endline (Fixed8.to_string (Fixed8.succ Fixed8.zero));
  print_endline (Fixed8.to_string (Fixed8.pred Fixed8.one));
  print_endline (Fixed8.to_string (Fixed8.min x8 y8));
  print_endline (Fixed8.to_string (Fixed8.max x8 y8));
  print_endline (string_of_bool (Fixed8.gth x8 y8));
  print_endline (string_of_bool (Fixed8.lth x8 y8));
  print_endline (string_of_bool (Fixed8.gte x8 y8));
  print_endline (string_of_bool (Fixed8.lte x8 y8));
  print_endline (string_of_bool (Fixed8.eqp x8 y8));
  print_endline (string_of_bool (Fixed8.eqs x8 y8));
  print_endline (Fixed8.to_string (Fixed8.add x8 y8));
  print_endline (Fixed8.to_string (Fixed8.sub x8 y8));
  print_endline (Fixed8.to_string (Fixed8.mul x8 y8));
  print_endline (Fixed8.to_string (Fixed8.div x8 y8))
