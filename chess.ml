open List
open Iterdeep

let chess_successors (x, y) =
  fold_left (fun ss (dx, dy) ->
	       let (x, y) = (x + dx, y + dy)
	       in
		 if x >= 1 && x <= 8 && y >= 1 && y <= 8 then
		   (x, y) :: ss
		 else
		   ss)
    []
    [(2, 1); (2, -1); (-2, 1); (-2, -1);
     (1, 2); (1, -2); (-1, 2); (-1, -2)];;

let path = iterative_deepening (1, 1) chess_successors (=) (8, 8)
in iter (fun (x, y) -> print_string "(" ; print_int x ; print_string "," ; print_int y ; print_string ") ") path ;
  print_newline ();;
