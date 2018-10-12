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
  type project = string * string * int
  let zero : project = ("", "", 0)
  let combine (x:project) (y:project) : project = zero
  let fail (x:project): project = zero
  let success (x:project): project = zero
end