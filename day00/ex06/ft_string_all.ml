
let is_digit c =
    c >= '0' && c <= '9'


let ft_string_all f str = 
    let len = String.length str in
    let rec loop_str i = 
        if i < len && (f (String.get str i)) then
            loop_str (i + 1)
        else
            i = len
    in loop_str 0


let main () =
    print_endline "is_digit 0123456789" ;
    if ft_string_all is_digit "0123456789" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_endline "is_digit 123a" ;
    if ft_string_all is_digit "123a" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_endline "is_digit a123" ;
    if ft_string_all is_digit "a123" then
        print_endline "True"
    else
        print_endline "False"

    ;

    print_endline "is_digit abc" ;
    if ft_string_all is_digit "abc" then
        print_endline "True"
    else
        print_endline "False"

let () = main ()
