let ft_is_palindrome str =
    let len = String.length str in
    let rec loop_str i j =
        if j < len && i <> j && String.get str i = String.get str j then
            loop_str (i + 1) (j - 1)
        else
            i = len
    in loop_str 0 len


let main () =
    print_string "radar -> " ;
    if ft_is_palindrome "radar" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_string "madam -> " ;
    if ft_is_palindrome "madam" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_string "car -> " ;
    if ft_is_palindrome "car" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_string " -> " ;
    if ft_is_palindrome "" then
        print_endline "True"
    else
        print_endline "False"


let () = main ()
