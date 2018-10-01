
let rec ft_power nb pow =
    if pow = 0 then
        1
    else
        begin
            if (pow mod 2) = 0 then
                let a = ft_power nb (pow / 2) in a * a
            else
                let a = ft_power nb ((pow - 1) / 2) in a * a * nb
        end


let main () =
    print_int (ft_power 2 4) ;
    print_char '\n' ;
    print_int (ft_power 3 0) ;
    print_char '\n' ;
    print_int (ft_power 0 5) ;
    print_char '\n' ;
    print_int (ft_power 10 3) ;
    print_char '\n' 


let () = main ()
