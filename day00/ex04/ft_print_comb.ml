let ft_print_comb () =
    let i = 0 in
    let j = 1 in
    let k = 2 in
    let rec comb i j k =
        print_int i;
        print_int j;
        print_int k;
        if i < 7 then
            print_string ", " ;
            if k < 9 then
                comb i j (k + 1)
            else if j < 8 then
                comb i (j + 1) (j + 2)
            else if i < 7 then
                comb (i + 1) (i + 2) (i + 3)
            else
                print_string "\n"
    in comb i j k


let main () =
    ft_print_comb ()


let () = main ()
