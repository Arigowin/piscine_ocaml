class dalek =
  object (self)

    val _name:string =
      "Dalek"
      ^ let randomName =
          ignore (Random.self_init());
          (String.make 1 (char_of_int ((Random.int 26) + (int_of_char 'A'))))
          ^ (String.make 1 (char_of_int ((Random.int 26) + (int_of_char 'a'))))
          ^ (String.make 1 (char_of_int ((Random.int 26) + (int_of_char 'a'))))
      in randomName
    val _hp:int = 100
    val mutable _shield:bool = true

    initializer Random.self_init()

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
      _shield <- if (_shield = true) then false else true
    method die = print_endline "Emergency temporal Shift!"

  end
