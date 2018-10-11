class virtual atom n s an =
  object (self)
    method name : string = n
    method symbol : string = s
    method atomic_number : int = an
    method to_string = "Atom " ^ self#name ^ "(" ^ self#symbol ^ "): " ^ (string_of_int self#atomic_number)
    method equals (a:atom) = self#atomic_number = a#atomic_number
    method compare (a:atom) = String.compare self#symbol a#symbol
  end
