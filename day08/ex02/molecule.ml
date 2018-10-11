class virtual molecule n atoms =
  object (self)
    method name : string = n
    method formula : string = self#set_formula_name atoms
    method to_string = "Molecule " ^ self#name ^ " (" ^ self#formula ^ ")"
    method equals (a:molecule) = self#formula = a#formula

    method private set_formula_name (atoms : Atom.atom list) =
      let sorted = 
        let compare_atoms (a1:Atom.atom) (a2:Atom.atom) =
          (* he Hill notation says we get carbon, then hydrogen, then everything else in alphabetical order *)
          if (String.compare a1#symbol "C") <> 0 && (String.compare a2#symbol "H") = 0 then
            1
          else
            a1#compare a2
        in List.sort compare_atoms atoms
      in match sorted with
      | []      -> ""
      | head::tail    -> 
        let rec formula_from_lst (a:Atom.atom) (atoms:Atom.atom list) c =
          match atoms with
          | []        -> a#symbol ^ (if c > 1 then (string_of_int c) else "")
          | h :: t    ->
            if a#equals h then
              formula_from_lst a t (c + 1)
            else
              a#symbol ^ (if c > 1 then (string_of_int c) else "") ^ formula_from_lst h t 1
        in formula_from_lst head tail 1
  end
