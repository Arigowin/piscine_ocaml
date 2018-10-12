module type Watchtower =
sig
  type hour = int
  val zero : hour
  val add : hour -> hour -> hour
  val sub : hour -> hour -> hour
end

module Watchtower =
struct
  type hour = int
  let zero : hour = 12
  let add (x: hour) (y: hour) : hour = 
    let res = (x + y) mod zero
    in if res = 0 then zero else res
  let sub (x: hour) (y: hour) : hour = 
    let (res:hour) = ((x - y) mod zero)
    in 
    if res = 0 then
      zero
    else if res < 0 then
      res + zero
    else
      res
end