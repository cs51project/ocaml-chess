module type BOARD = 
sig
  type piece_position
  type piece_type
  type piece_color
  type piece
  type move = Move of ( piece_type * piece_position ) | Castle of castle  
  type board

  val init_board : board (* standard starting board *)

  val all_moves : board -> move list 
    (* all valid moves *)
  val all_pieces : board -> piece list 
    (* all pieces on current board *)
  val move : board -> move -> board option 
    (* should return None if the move is invalid *)
  val check : dict -> color option 
    (* returns color in position to win *)
  val checkmate: dict -> color option
    (* checks whether the game is over *)
end

module ChessSet(B: BOARD) : (SET with type piece = B.t) = 
  struct
    type piece_position = int * int
    type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
    type piece_color = Black | White
    type piece = {name : piece_type; color: piece_color; position: piece_position}
    type move = Move of ( piece_type * piece_position ) | Castle of castle 
    
   end
