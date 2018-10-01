
let ft_print_rev str =
    let i = (String.length str) in
    let rec loop_str i =
        if i >= 0 then
            begin
                print_char (String.get str i) ;
                loop_str (i - 1)
            end
        else
            print_char '\n'
    in loop_str (i - 1)


let main () =
    ft_print_rev "Hello world !" ;
    ft_print_rev "cba" ;
    ft_print_rev "! dlrow olleH" ;
    ft_print_rev ""


let () = main ()
