open List

let rec first_success f list =
  match list with
      [] -> None
    | x :: xs ->
	match f x with
	    Some _ as result -> result
	  | None -> first_success f xs;;

(* iterative_deepening 'state			start state
   		       'state -> 'state list	successor function
   		       'state -> state -> bool	state equality
		       'state			goal state
   -> 'state list
*)
let rec iterative_deepening start successors equal goal =
  let rec deepen state depth =
    if equal state goal then
      Some [state]
    else if depth = 0 then
      None
    else
      let succs = (successors state)
      in let result = first_success (fun s -> deepen s (depth - 1)) succs
      in match result with
	  Some path -> Some (state :: path)
	| None -> None
  and iterate depth =
    match deepen start depth with
	Some path -> path
      | None -> iterate (depth + 1)
  in
    iterate 0;;
