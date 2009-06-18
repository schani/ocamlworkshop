type person = Isabell | Susanne | Maja | Sieglinde
and state = Torch_source of person list * person list
	    | Torch_dest of person list * person list;;

let print_person p =
  match p with
      Isabell -> print_string "Isabell"
    | Susanne -> print_string "Susanne"
    | Maja -> print_string "Maja"
    | Sieglinde -> print_string "Sieglinde";;

let rec print_people ps =
  match ps with
      [] -> ()
    | p :: rest -> print_person p ; print_string " " ; print_people rest;;

let print_state = function
    Torch_source (source, dest) ->
      print_string "* " ; print_people source ; print_string "| " ; print_people dest
  | Torch_dest (source, dest) ->
      print_people source ; print_string "| " ; print_people dest ; print_string "*";;

let remove l i =
  List.filter (function j -> i <> j) l;;

let rec diff l m =
  match m with
      [] -> l
    | x :: xs -> diff (remove l x) xs;;

let speed p =
  match p with
      Isabell -> 5
    | Susanne -> 10
    | Maja -> 20
    | Sieglinde -> 25;;

let rec map_two f l =
  match l with
      [] -> []
    | x :: xs ->
	List.append (List.map (function y -> f x y) xs)
	  (map_two f xs);;

let successors state =
  match state with
      Torch_source (source, dest) ->
	map_two (fun p q ->
		   (Torch_dest (diff source [p;q], p :: q :: dest), max (speed p) (speed q)))
	  source
    | Torch_dest (source, dest) ->
	List.map (function p ->
		    (Torch_source (p :: source, remove dest p), speed p))
	  dest;;

let state_source = function
    Torch_source (source, _) -> source
  | Torch_dest (source, _) -> source;;

let costs state =
  List.fold_left max 0 (List.map speed (state_source state));;

let sets_equal s1 s2 =
  ((List.length s1) = (List.length s2))
  &&
    List.for_all (function i -> List.mem i s2) s1;;

let equal s1 s2 =
  match (s1, s2) with
      (Torch_source (s1, _), Torch_source (s2, _)) -> sets_equal s1 s2
    | (Torch_dest (s1, _), Torch_dest (s2, _)) -> sets_equal s1 s2
    | _ -> false;;

let start = Torch_source ([Isabell; Susanne; Maja; Sieglinde], [])
and goal = Torch_dest ([], [Isabell; Susanne; Maja; Sieglinde])
in let result_path = List.rev (Astar.astar start successors costs equal goal)
in List.iter (function state -> print_state state ; print_newline ()) result_path;;
