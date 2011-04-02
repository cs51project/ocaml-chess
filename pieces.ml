open Board

(* Does not check if King is in 'check' when generating moves. Must 
 * do that before passing position to this function. Does not generate moves
 * for passing a pawn to the other side.*)

(*let generate_moves (pos: position) (pt:piece_type) (p:piece) (b:board) 
    (base:position list): position list = 
    match pos with 
      | Pos(x,y) -> *)

  let same_side pc1 pc2 =
    match (pc1, pc2) with
      | (Black _, Black _) | (White _, White _) -> true
      | (Black _, White _) | (White _, Black _) -> false
  
  (* returns list of positions in a given direction from a position *)
  let rec direction (pc:piece) (pos:position) (lst:position list) (b:board)
      (j: int) (i:int) (lim:int) : position list = 
    if lim < 0 then lst
    else
      let new_pos_opt = neighbor j i pos in
      match new_pos_opt with
	| None -> lst
	| Some new_pos ->
        match lookup new_pos b with
          | None ->
              direction new_pos (new_pos::lst) b j i (lim - 1)
          | Some pc2 -> 
              if same_side pc pc2 then lst else new_pos::lst
 

  let pawn_moves (pos:position) (pc:piece) (b:board) = 
    if pc = Black then let j = -1 else let j = 1 in
      let m = lookup (deopt_pos (neighbor j 0 pos) b in
      let r = lookup (deopt_pos (neighbor j 1 pos)) b in 
      let l = lookup (deopt_pos (neighbor j -1 pos) b in
      let mid = match m with 
	|None -> (create_pos x y+j)::base
	|Some _ -> base in
      let frontright = match r with 
	|None -> base
	|Some color -> if p = color then base else (create_pos x+1 y+j) in
      let frontleft = match l with 
	|None -> base
	|Some color -> if p = color then base else (create_pos x-1 y+j) in
      frontleft @ mid @ frontright in
       
  let rook_moves lim = 
    (direction x y base b 1 0 lim) @ 
    (direction x y base b 0 1 lim) @
    (direction x y base b -1 0 lim) @
    (direction x y base b 0 -1 lim) in
  
  let bishop_moves lim = 
    (direction x y p base b 1 1 lim) @
    (direction x y p base b -1 1 lim) @
    (direction x y p base b 1 -1 lim) @
    (direction x y p base b -1 -1 lim) @ in
  
  let knight_moves () = 
    (direction x y p base b 1 2 1) @
    (direction x y p base b 2 1 1) @
    (direction x y p base b -1 2 1) @
    (direction x y p base b -2 1 1) @
    (direction x y p base b 1 -2 1) @
    (direction x y p base b 2 -1 1) @
    (direction x y p base b -1 -2 1) @
    (direction x y p base b -2 -1 1) @ in


  match pt with
    |Pawn   -> pawn_moves 
    |Rook   -> rook_moves 9
    |Knight -> knight_moves 
    |Bishop -> bishop_moves 9
    |Queen  -> (rook_moves 9) @ (bishop_moves 9)
    |King   -> (rook_moves 1) @ (bishop_moves 1) in
generate_moves pos pt p b []

