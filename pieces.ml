module ChessSet(B: BOARD) : (SET with type piece = B.t) = 
  struct
    open Board ;;
    type board = piece list;;
    type piece_type = B.piece_type ;;
    type init_board = 
    
   end
