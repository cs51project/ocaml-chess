open Board

(* Does not check if King is in 'check' when generating moves. Must 
 * do that before passing position to this function. *)

let generate_moves (pos: position) (pt:piece_type) (p:piece) (b:board) 
    (base:position list): position list = 
  match pos with 
    |Pos(x,y) -> 	   

  let rec direction (x:int) (y:int) (lst:position list) (b:board)
      (i: int) (j:int) (lim:int) = 
    if lim < 0 | x+i < 0 | x+i > 7 | y+j < 0 | y+j > 7  then lst else(
      match lookup (create_pos x+j y+k) b with
	| None -> direction x+i y+j ((create_pos x+i y+j)::lst) b i j (lim-1)
	| Some color -> if p = color then lst else (create_pos x+i y+j)::lst) in

  let pawn_moves () = if p = Black then let j = -1 else let j = 1 in
      let m = lookup (create_pos x y+j) b in
      let r = lookup (create_pos x+1 y+j) b in 
      let l = lookup (create_pos x-1 y+j) b in
      let mid = match m with 
	|None -> (create_pos x y+j)::base
	|Some _ -> base in
      let right = match r with 
	|None -> base
	|Some color -> if p = color then base else (create_pos x+1 y+j) in
      let left = match l with 
	|None -> base
	|Some color -> if p = color then base else (create_pos x-1 y+j) in
      left @ mid @ right in
       
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

