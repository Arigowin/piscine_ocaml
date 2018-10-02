let ft_print_alphabet () =
    let rec loop_letters c =
        if c <= 122 then
            begin
                print_char (char_of_int c) ;
                loop_letters (c + 1)
            end
        else
            print_char '\n' in
    loop_letters 97


let main () =
    ft_print_alphabet ()


let () = main ()
