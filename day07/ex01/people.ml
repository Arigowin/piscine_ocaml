class people name =
  object
    val _name:string = name
    val _hp:int = 100

    initializer print_endline ("I'm a people, and my name is " ^ _name)

    method to_string = _name ^ " has " ^ (string_of_int _hp) ^ " HP."
    method talk = print_endline ("I'm " ^ _name ^ "! Do you know the Doctor?")
    method die = print_endline "Aaaarghh!"
  end