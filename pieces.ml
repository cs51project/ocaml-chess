module ChessSet(B: BOARD) : (SET with type piece = B.t) = 
  struct
    open Board ;;
    type board = piece list;;
    type piece_type = B.piece_type ;;
    type init_board = 
    
   end


let rec up x y p lst = 
  if y+1 > 7 then lst else(
     match MapBoard.lookup (x,y+1) b with
       | None -> up y+1 (x,y+1)::lst
       | Some color -> if p = color then lst else (x,y+1)::lst ) 
;;
let rec down x y p lst = 
  if y-1 < 0 then lst else(
     match MapBoard.lookup (x,y-1) b with
       | None -> down y-1 (x,y-1)::lst
       | Some color -> if p = color then lst else (x,y-1)::lst ) 
;;     
let rec right x y p lst = 
  if x+1 > 7 then lst else(
    match MapBoard.lookup (x+1,y) b with
      | None -> right x+1 (x+1,y)::lst
      | Some color -> if p = color then lst else (x+1,y)::lst ) 
;;
let rec left x y p lst = 
  if x-1 < 0 then lst else(
    match MapBoard.lookup (x-1,y) b with
      | None -> left x-1 (x-1,y)::lst
      | Some color -> if p = color then lst else (x-1,y)::lst )
;;






let generate_move (pos: position) (pt:piece_type) (p:piece) (b:board) 
    (base:position list): position list = 

  (* Checks position for a blocking piece returns BOOL *)
  let check_p p1 = ((MapBoard.lookup p1 b) = p) in
	   
  let pawn_moves () = 
    match pos with 
      |(x,y) -> if p = Black then (x,y-1)::base else (x,y+1)::base in 

  let rook_moves () =
    match pos with
      |(x,y) -> (up x y base)@(down x y base)@(left x y base)@(right x y base) in
	 
	
  

  match pt with
    |Pawn   -> pawn_moves  
    |Rook   -> rook_moves
    |Knight ->
    |Bishop ->
    |Queen  ->
    |King   ->

