open Board

(* Does not check if King is in 'check' when generating moves. Must 
 * do that before passing position to this function. Does not generate moves
 * for passing a pawn to the other side.*)

(*let generate_moves (pos: position) (pt:piece_type) (p:piece) (b:board) 
    (base:position list): position list = 
    match pos with 
      | Pos(x,y) -> *)

  let same_side (pc1:piece) (pc2:piece) =
    match (pc1, pc2) with
      | (Black _, Black _) | (White _, White _) -> true
      | (Black _, White _) | (White _, Black _) -> false
  
  (* returns list of positions in a given direction from a position *)
  let rec direction (pc:piece) (pos:position) (lst:position list) (b:board)
      (j: int) (i:int) (lim:int) : position list = 
    if lim < 0 then lst
    else
      match (neighbor j i pos) with
	| None -> lst
	| Some new_pos ->
                 match lookup (Some new_pos) b with
		   | None ->
		     direction pc new_pos (new_pos::lst) b j i (lim - 1)
		   | Some pc2 -> 
		     if same_side pc pc2 then lst else new_pos::lst
		

  let pawn_moves (pc:piece) (pos:position) (b:board) = 
    if pc = Black then let j = -1 else let j = 1 in
      let m = lookup (neighbor j 0 pos) b in
      let r = lookup (neighbor j 1 pos) b in 
      let l = lookup (neighbor j -1 pos) b in
      match pos with
	|Pos(x,y) ->
	  let mid = match m with 
	    | None -> (create_pos x y+j)
	    | Some _ -> [] in
	  let frontright = match r with 
	    | None -> []
	    | Some color -> if p = color then base else (create_pos x+1 y+j) in
	  let frontleft = match l with 
	    | None -> []
	    | Some color -> if p = color then base else (create_pos x-1 y+j) in
	  frontleft @ mid @ frontright 
       
  let rook_moves (pc:piece) (pos:position) (b:board) (lim:int) = 
    (direction pc pos [] b 1 0 lim) @ 
    (direction pc pos [] b 0 1 lim) @
    (direction pc pos [] b -1 0 lim) @
    (direction pc pos [] b 0 -1 lim) 
  
  let bishop_moves (pc:piece) (pos:position) (b:board) (lim:int) = 
    (direction pc pos p [] b 1 1 lim) @
    (direction pc pos p [] b -1 1 lim) @
    (direction pc pos p [] b 1 -1 lim) @
    (direction pc pos p [] b -1 -1 lim) 
  
  let knight_moves (pc:piece) (pos:position) (b:board) = 
    (direction pc pos [] b 1 2 1) @
    (direction pc pos [] b 2 1 1) @
    (direction pc pos [] b -1 2 1) @
    (direction pc pos [] b -2 1 1) @
    (direction pc pos [] b 1 -2 1) @
    (direction pc pos [] b 2 -1 1) @
    (direction pc pos [] b -1 -2 1) @
    (direction pc pos [] b -2 -1 1) 

  let queen_moves (pc:piece) (pos:position) (b:board) (lim:int) =
    (rook_moves pc pos b lim) @ (bishop_moves pc pos b lim)

  let generate_moves (pt:piece_type) (pc:piece) (pos:position) (b:board)
  match pt with
    |Pawn   -> pawn_moves pc pos b
    |Rook   -> rook_moves pc pos b 9
    |Knight -> knight_moves pc pos b
    |Bishop -> bishop_moves pc pos b 9
    |Queen  -> queen_moves pc pos b 9
    |King   -> queen_moves pc pos b 1 in
  generate_moves pos pt p b 

