let print_int_endline i =
    print_int i ;
    print_char '\n'


let rec ft_countdown start =
    if start <= 0 then
        print_int_endline 0
    else
        begin
            print_int_endline start;
            ft_countdown (start - 1)
        end


let main () =
    ft_countdown 3 ;
    print_char '\n' ;
    ft_countdown 0 ;
    print_char '\n' ;
    ft_countdown (-1) ;
    print_char '\n' ;
    ft_countdown 10


let () = main ()
