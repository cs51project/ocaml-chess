module type BOARD = 
sig
  type piece_position
  type piece_type
  type piece_color
  type piece
  type move 
  type board

  val init_board : unit -> board (* standard starting board *)

  val all_moves : board -> move list 
    (* all valid moves *)
  val all_pieces : board -> piece list 
    (* all pieces on current board *)
  val capture : play -> piece option (* returns piece captured or None *)
  val play : board -> move -> board option 
    (* should return None if the move is invalid *)
  val check : dict -> color option 
    (* returns color in position to win *)
  val checkmate: dict -> color option
    (* checks whether the game is over *)
end

module ChessSet(B: BOARD) : (SET with type piece = B.t) = 
  struct
    open Pieces;;
    type piece_position = int * int (* 0,0--lower lt; 7,7--upper rt *)
    type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
    type piece_color = Black | White
    type piece = {name : piece_type; color: piece_color; position: piece_position}
    type move = Move of ( piece_type * piece_position ) | Castle of castle    type board = piece list (* list of active pieces *)
    let init_board () =  [wr1;...;br2];
    let all_moves (board: board) : move list = [Castle]; (*incomplete*)
    
   end
