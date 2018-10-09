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
    type t = (int * int)
    let of_float (x:float) = (Fb.bits, int_of_float (x *. float_of_int (1 lsl Fb.bits)))
    let of_int x = (0, 0)
    let to_float x = (float_of_int x) / (1 lsr Fb.bits)
    let to_int x = 0
    let to_string x = ""
    let zero = (0, 0)
    let one = (0, 0)
    let succ x = (0, 0)
    let pred x = (0, 0)
    let min x1 x2 = (0, 0)
    let max x1 x2 = (0, 0)
    let gth x1 x2 = true
    let lth x1 x2 = true
    let gte x1 x2 = true
    let lte x1 x2 = true
    let eqp x1 x2 = x1 == x2
    let eqs x1 x2 = x1 = x2
    let add x1 x2 = (0, 0)
    let sub x1 x2 = (0, 0)
    let mul x1 x2 = (0, 0)
    let div x1 x2 = (0, 0)
    let foreach x1 x2 f = ()
  end


(****************************************************************************************)

module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f))
