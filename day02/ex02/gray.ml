let gray n =
    let rec abc acc n = match n with
    | 0 -> acc
    | _ -> abc (acc :: 0) (n - 1)
    in let rec def l =

    in abc [] n;;


let main () =
    gray 3;;


let () = main ();;
