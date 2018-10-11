class virtual atom =
  object (self)
    method virtual name :string
    method virtual symbol : string
    method virtual atomic_number : int
    method to_string = "Atom " ^ self#name ^ "(" ^ self#symbol ^ "): " ^ (string_of_int self#atomic_number)
    method equals (a:atom) = self#atomic_number = a#atomic_number
  end
