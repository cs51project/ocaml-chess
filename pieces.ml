module ChessSet(B: BOARD) : (SET with type piece = B.t) = 
  struct
    open Board ;;
    type board = piece list;;
    type piece_type = B.piece_type ;;
    type init_board = 
    
   end

let generate_move (pos: position) (pt:piece_type) (p:piece) (b:board) 
    (base:position list): position list = 
  match pos with 
    |(x,y) ->	   

  let rec direction (x:int) (y:int) (lst:position list) (b:board)
      (i: int) (j:int) = 
    if x+i < 0 | x+i > 7 | y+j < 0 | y+j > 7  then lst else(
      match MapBoard.lookup (x+j,y+k) b with
	| None -> direction x+i y+j (x+i,y+j)::lst b i j
	| Some color -> if p = color then lst else (x+i,y+j)::lst) in

  let pawn_moves () = if p = Black then let j = -1 else let j = 1 in
      let m = Mapboard.lookup (x,y+j) in
      let r = Mapboard.lookup (x+1,y+j) in 
      let l = Mapboard.lookup (x-1,y+j) in
      let mid	match m with 
	|None -> (x,y+j)::base
	|Some _ -> base in
      let right match r with 
	|None -> base
	|Some color -> if p = color then base else (x+1,y+j) in
      let left match l with 
	|None -> base
	|Some color -> if p = color then base else (x-1,y+j) in
      left::mid::right in
       
  let rook_moves () = (direction x y base b 1 0) @ 
                      (direction x y base b 0 1) @
	              (direction x y base b -1 0) @
	              (direction x y base b 0 -1) in
  
  let bishop_moves () = (direction x y p base b 1 1) @
	                (direction x y p base b -1 1) @
	                (direction x y p base b 1 -1) @
	                (direction x y p base b -1 -1) @ in
	
  

  match pt with
    |Pawn   -> pawn_moves  
    |Rook   -> rook_moves
    |Knight ->
    |Bishop -> bishop_moves
    |Queen  -> rook_moves @ bishop_moves
    |King   ->

