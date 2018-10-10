class ['a] army =
  object

    val _lst : 'a list = []

    method add (e:'a) = {< _lst = e :: _lst >}

    method delete (e:'a) = 
      let lst_remove x lst = 
        let rec loop x acc = function
          | [] -> List.rev acc
          | a :: b when a = x -> loop x acc b
          | a :: b -> loop x (a :: acc) b
        in loop x [] lst
      in {< _lst = lst_remove e _lst >}

  end