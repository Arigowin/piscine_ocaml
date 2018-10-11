class alkane n =
  object (self)
    method name = (self#gen_name n) ^ "ane"
    method formula = self#gen_formula n

    method private gen_formula n =
      if n > 0 && n <= 12 then
        "C" ^ (if n > 1 then (string_of_int n) else "") ^ "H" ^ (string_of_int ((n * 2) + 2))
      else
        "Invalid alkane"

    method private gen_name = function
      | 1     -> "Meth"
      | 2     -> "Eth"
      | 3     -> "Prop"
      | 4     -> "But"
      | 5     -> "Pent"
      | 6     -> "Hex"
      | 7     -> "Hept"
      | 8     -> "Oct"
      | 9     -> "Non"
      | 10    -> "Dec"
      | 11    -> "Undec"
      | 12    -> "Dodec"
      | _     -> "Invalide alk"

    method to_string = "Alkane " ^ self#name ^ " (" ^ self#formula ^ ")"
    method equals (a:alkane) = (self#name = a#name) && (self#formula = a#formula)
  end
