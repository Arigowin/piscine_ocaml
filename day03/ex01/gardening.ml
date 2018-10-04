type 'a tree = Nil
             | Node of 'a * 'a tree * 'a tree


let draw_square x y size =
  let x_origine = (x - (size / 2)) in
  let y_origine = (y + (size / 2)) in
  Graphics.moveto x_origine y_origine;                      (* move to x y *)
  Graphics.lineto (x_origine + size) y_origine;             (* draw up line *)
  Graphics.lineto (x_origine + size) (y_origine - size);    (* draw right line *)
  Graphics.lineto x_origine (y_origine - size);             (* draw down line *)
  Graphics.lineto x_origine y_origine                       (* draw left line *)


let draw_tree tree =
  let size = 50
  in let rec browse x y = function
      | Node(v, l1, l2) ->
        begin
          draw_square x y size;
          Graphics.moveto (x - (size / 3)) y;
          Graphics.draw_string v;
          Graphics.moveto (x + size / 2) y;
          Graphics.lineto (x + 100 - (size / 2)) (y + 50);
          browse (x + 100) (y + 50) l1;
          Graphics.moveto (x + size / 2) y;
          Graphics.lineto (x + 100 - (size / 2)) (y - 50);
          browse (x + 100) (y - 50) l2;
        end
      | Nil ->
        (
          draw_square x y size; 
          Graphics.moveto (x - (size / 3)) y; 
          Graphics.draw_string "Nil";
        )
  in browse 300 300 tree


let rec size = function
  | Node(_, l1, l2) -> 1 + (size l1) + (size l2)
  | Nil -> 0


let rec height = function
  | Node(_, l1, l2) -> 
    let max i j = if i < j then j else i in
    max (height l1 + 1) (height l2 + 1)
  | Nil -> 0


let main () =
  (* init *)
  Graphics.open_graph " 800x600";

  (* test *)
  let t = (Node ("Test", (Node ("Test2", (Node ("Test3", Nil, Nil)), Nil)), Nil));
  in print_int (height t);
  print_char '\n';
  print_int (size t);
  print_char '\n';
  draw_tree t;
  ignore(Graphics.read_key ())

let () = main ()
