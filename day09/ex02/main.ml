module type MONOID =
sig
  type element
  val zero1 : element
  val zero2 : element
  val mul : element -> element -> element
  val add : element -> element -> element
  val div : element -> element -> element
  val sub : element -> element -> element
end

module type Calc =
  functor (M : MONOID) ->
  sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
  end

(**********************************************************)

module INT =
struct
  type element = int
  let zero1 : element = 0
  let zero2 : element = 1
  let add (x:element) (y:element) : element = x + y
  let sub (x:element) (y:element) : element = x - y
  let mul (x:element) (y:element) : element = x * y
  let div (x:element) (y:element) : element = x * y
end

module FLOAT =
struct
  type element = float
  let zero1 : element = 0.0
  let zero2 : element = 1.0
  let add (x:element) (y:element) : element = x +. y
  let sub (x:element) (y:element) : element = x -. y
  let mul (x:element) (y:element) : element = x *. y
  let div (x:element) (y:element) : element = x /. y
end

module Calc =
  functor (M: MONOID) ->
  struct
    let add (x:M.element) (y:M.element) : M.element = M.add x y
    let sub (x:M.element) (y:M.element) : M.element = M.sub x y
    let mul (x:M.element) (y:M.element) : M.element = M.mul x y
    let div (x:M.element) (y:M.element) : M.element = M.div x y
    let power (x:M.element) (pow:int) : M.element =
      if pow = 0 then M.zero2 else
        let rec ft_power x1 pow2 =
          if pow2 = 0 then x1
          else ft_power (mul x1 x) (pow2 - 1)
        in ft_power x (pow - 1)
    let fact (x:M.element) : M.element =
      if x = M.zero1 then M.zero2 else
        let rec ft_fact acc n =
          if n = M.zero1 then acc
          else ft_fact (mul n acc) (sub n M.zero2)
        in ft_fact x (sub x M.zero2)
  end

module Calc_int = Calc(INT)
module Calc_float = Calc(FLOAT)
let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_int (Calc_int.power 3 0));
  print_endline (string_of_int (Calc_int.power 10 2));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_float (Calc_float.power 3.0 0));
  print_endline (string_of_float (Calc_float.power 10.1 2));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  print_endline (string_of_int (Calc_int.fact 5));
  print_endline (string_of_int (Calc_int.fact 1));
  print_endline (string_of_int (Calc_int.fact 0));
  print_endline (string_of_int (Calc_int.add 1 2));
  print_endline (string_of_float (Calc_float.add 1.1 2.2));
  print_endline (string_of_int (Calc_int.sub 1 2));
  print_endline (string_of_float (Calc_float.sub 1.1 2.2));
  print_endline (string_of_int (Calc_int.div 10 2));
  print_endline (string_of_float (Calc_float.div 10.1 2.0));
