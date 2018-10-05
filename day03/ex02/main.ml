let main () = 
  (* Uncipher.unrot42 "def"; *)
  print_endline (Cipher.ft_crypt "Salut Les Amies" [Cipher.rot42; (Cipher.caesar 12); (Cipher.xor 2)]);
  print_endline (Uncipher.ft_uncrypt "Walut\"Lew\"Amiew" [(Uncipher.xor 2); (Uncipher.uncaesar 12); Uncipher.unrot42])


let () = main ()