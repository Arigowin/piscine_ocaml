module Try =
struct
  type 'a t = Success of 'a | Failure of exn

  let return monad = Success monad

  let bind monad (f: ('a -> 'b t)) =
    match monad with
    | Success (x)               -> f x
    | Failure (x)               -> Failure x

  let recover monad (f : (exn -> 'a t)) = 
    match monad with
    | Success (x)               -> Success x
    | Failure (x)               -> f x

  let filter monad (f: 'a -> bool) =
    match monad with
    | Success (x) when (f x)    -> Success x
    | Success (x)               -> Failure (Invalid_argument "Failure")
    | Failure (x)               -> Failure x

  let flatten monad =
    match monad with
    | Success (x)               ->
      (match x with
       | Success (x2)       -> Success x2
       | Failure (x2)       -> Failure x2)
    | Failure (x)               -> Failure x
end

let () =
  let testExc test =
    match test with
    | Try.Success (x)   -> print_endline "Success"
    | Try.Failure (x)   -> print_endline (Printexc.to_string x)
  in let t1 = Try.return 10 in
  let t2 = Try.filter t1 (fun x -> false)
  in let t3 = Try.bind t1 (fun x -> Try.return (Try.filter (Try.return (x + 1)) (fun x -> false)))
  in testExc t1;
  testExc t2; 
  testExc t3; 
  testExc (Try.flatten t3);
  testExc (Try.recover t2 (fun ex -> Try.Failure (Invalid_argument "Test exception")))
