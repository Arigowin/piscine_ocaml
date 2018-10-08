let read_file path : string list =
  let ic = open_in path
  in let rec loop acc =
       try
         let s = input_line ic
         in loop ([s] @ acc)
       with
       | Sys_error err -> [Printf.sprintf "Something went wrong: %s\n" err]
       | End_of_file -> close_in ic; acc
  in loop []


let examples_of_file (path:string) : (float array * string) list =
  let lines = read_file path
  in let rec loop acc


(* 1.0,0.5,0.3,g will be converted to ([|1.0; 0.5 ;0.3 |], "g") *)

let main () =
  print_string "Coucou"


let () = main ()
