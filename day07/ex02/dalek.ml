class dalek =
  object (self)

    val _name:string = 
      let rec randStr i acc =
        if i <= 0 then
          acc
        else
          let c = 
            if (Random.int 2) = 0 then
              char_of_int (Random.int (26 + 97))
            else
              char_of_int (Random.int (26 + 65))
          in (randStr (i - 1) (acc ^ (String.make 1 c)))
      in
      randStr 3 "Dalek"
    val _hp:int = 100
    val mutable _shield:bool = true

    method to_string = _name ^ " has " ^ (string_of_int _hp) ^ " HP and has " ^ (if _shield then "shield." else "no shield.")
    method talk = 
      print_endline (match (Random.int 4) with
          | 0 -> "Explain! Explain!"
          | 1 -> "Exterminate! Exterminate!"
          | 2 -> "I obey!"
          | 3 -> "You are the Doctor! You are the enemy of the Daleks!"
          | _ -> "")
    method exterminate (p:People.people) =
      p#die;
      _shield <- if _shield = true then false else true
    method die = print_endline "Emergency temporal Shift!"
  end