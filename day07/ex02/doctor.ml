class doctor name age sidekick =
  object

    val _name:string = name
    val _age:int = age

    val _sidekick:People.people = sidekick
    val _hp = 100

    initializer print_endline ("I'm the Doctor")

    method to_string = "Doctor " ^ _name ^ " aged " ^ (string_of_int _age) ^ " with sidekick " ^ _sidekick#to_string ^ " and HP " ^ (string_of_int _hp)
    method talk = print_endline "Hi! I'm the Doctor"
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method private regenerate = new doctor _name _age _sidekick
    method travel_in_time (start:int) (arrival:int) =
      (print_endline "              _\n             /-\\\n        _____|#|_____\n       |_____________|\n      |_______________|\n    |||_POLICE_##_BOX_|||\n     | |¯|¯|¯|||¯|¯|¯| |\n     | |-|-|-|||-|-|-| |\n     | |_|_|_|||_|_|_| |\n     | ||~~~| | |¯¯¯|| |\n     | ||~~~|!|!| O || |\n     | ||~~~| |.|___|| |\n     | ||¯¯¯| | |¯¯¯|| |\n     | ||   | | |   || |\n     | ||___| | |___|| |\n     | ||¯¯¯| | |¯¯¯|| |\n     | ||   | | |   || |\n     | ||___| | |___|| |\n    |¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯|\n     ¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯¯";
       new doctor _name (_age + (arrival - start)) _sidekick)
  end