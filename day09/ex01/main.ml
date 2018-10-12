let () =
  let p1 : App.App.project = ("a", "", 0)
  in let p2 : App.App.project = ("b", "", 0)
  in let ps = App.App.success p1
  in let pf = App.App.fail p2
  in let res = App.App.combine ps pf
  in App.print_proj res;
  App.print_proj p1;
  App.print_proj p2;
  App.print_proj ps;
  App.print_proj pf
