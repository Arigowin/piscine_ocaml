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


let draw_tree_node node =
  let size = 50
  in let rec browse x y = function
      | Node(v, l1, l2) ->
        begin
          draw_square x y size;
          Graphics.moveto (x - (size / 3)) y;
          Graphics.draw_string v;
          Graphics.moveto (x + size / 2) y;
          Graphics.lineto (x + 100 - (size / 2)) (y + 30);
          browse (x + 100) (y + 30) l1;
          Graphics.moveto (x + size / 2) y;
          Graphics.lineto (x + 100 - (size / 2)) (y - 30);
          browse (x + 100) (y - 30) l2;
        end
      | Nil ->
        (
          draw_square x y size; 
          Graphics.moveto (x - (size / 3)) y; 
          Graphics.draw_string "Nil";
        )
  in browse 300 300 node;;

let main () =
  Graphics.open_graph " 800x600";
  let t = (Node ("Test", (Node ("Test2", Nil, Nil)), Nil));
  in draw_tree_node t;
  Graphics.read_key ();;

let _ = main();;
