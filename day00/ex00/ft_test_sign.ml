
let ft_test_sign nb =
    if nb >= 0 then
        print_endline "positive"
    else
        print_endline "negative"

let main () =
    print_endline "42" ;
    ft_test_sign 42 ;

    print_endline "0" ;
    ft_test_sign 0 ;

    print_endline "-42" ;
    ft_test_sign (-42) ;

    print_endline "-0" ;
    ft_test_sign (-0)


let () = main ()
