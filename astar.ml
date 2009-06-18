type 'a astar_state = { path: 'a list ; costs_so_far: int };;

(* astar 'state                            start state
         'state -> (('state * int) list)   successor function
         'state -> int                     cost function h(x)
         'state -> 'state -> bool          state equality
         'state                            goal state
     -> 'state list

   The successor function returns a list of successor states and the
   costs for each such state transition.

   astar returns a list containing all the paths from goal to start,
   or throws an exception if no path is found.
*)
let astar start successors costs equal goal =
  let rec work queue closed =
    let (_, { path = p ; costs_so_far = gp }, queue) = Pqueue.extract queue
    in let x = List.hd p
    in 
      if List.exists (equal x) closed then
	work queue closed
      else if equal x goal then
	p
      else
	let closed = x :: closed
	and queue =
	  List.fold_left (function q -> function (y, cy) ->
			    Pqueue.insert q (gp + cy + (costs y))
			      { path = (y :: p) ; costs_so_far = gp + cy })
	    queue (successors x)
	in work queue closed
  in
    work (Pqueue.insert Pqueue.empty (costs start) { path =  [start] ; costs_so_far = 0 }) [];;
