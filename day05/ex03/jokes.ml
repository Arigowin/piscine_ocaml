let read_file file : string list =
  let ic = open_in file
  in let rec loop acc =
       try
         let s = input_line ic
         in loop ([s] @ acc)
       with
       | Sys_error err -> [Printf.sprintf "Something went wrong: %s\n" err]
       | End_of_file -> close_in ic; acc
  in loop []


let create_array_of_jokes file =
  let list_of_jokes = read_file file;
  in Array.init (List.length list_of_jokes) (fun x -> (List.nth list_of_jokes x))

let ft_jokes file =
  Random.self_init();
  let jokes = create_array_of_jokes file
  in print_endline jokes.(Random.int (Array.length jokes))


let main av =
  if Array.length av = 2 then
    ft_jokes av.(1)

let () =
  main Sys.argv
