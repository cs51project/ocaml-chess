module type BOARD = 
sig
  (* A1 is (0, 0); H8 is (7, 7) *)
  type position = int * int
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  (* encode Black as Black King, White as White King *)
  type side = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type board

  (* standard starting board *)
  val init_board : board
  (* which color is to play *)
  val whose_turn : board -> side
  (* is move valid? *)
  val is_valid : board -> move -> bool
  (* all valid moves *)
  val all_moves : board -> move list
  (* all positions on current board *)
  val all_pieces : board -> (position, piece) list
  (* what piece is at given position *)
  val piece_at : board -> position -> piece option
  (* should return None if the move is invalid *)
  val play : board -> move -> board option
  (* returns color in check or None *)
  val check : board -> side option
  (* returns losing color or None *)
  val checkmate: board -> side option
end



module MapBoard : BOARD =
struct
  type position = int * int
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  type side = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle

  module PositionMap = Map.Make(struct
      type t = position
      let compare p1 p2 =
        let (p1, p2) = ((p1r, p1f), (p2r, p2f)) in
          if p1r < p2r then -1
          else if p1r > p2r then 1
          else if p1r = p2r then
            if p1f < p2f then -1
            else if p1f > p2f then 1
            else 0
    end)

  (* a board is a map of location => piece together with a side 'to play' *)
  type board = (piece PositionMap.t) * side 
  
  let init_board = 
    let files = [0; 1; 2; 3; 4; 5; 6; 7] in
    let names = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
    let pc_files = List. combine files names in
    let init_pcs = List.fold_left
      (fun r (x, pc) -> ((x, 7), Black pc) :: ((x, 0), White pc) :: r) [] pc_files in
    let init_pawns = List.fold_left 
      (fun r x -> ((x, 6), Black Pawn) :: ((x, 1), White Pawn) :: r) [] files in
    let init_bindings = init_pcs @ init_pawns in
    let add_binding board ((x, y), pc) =
      PositionMap.add (x, y) pc board
    in
      (List.fold_left add_binding PositionMap.empty init_bindings, White King)

  let flip_side (s: side) =
    match s with
      | White _ -> Black _
      | Black _ -> White _

  let all_pieces b =
    let (map, side) = b in
      PartitionMap.bindings map

  let play b m =
    if is_valid b m then
      let (map, side) = b in
        match m with
          | Standard (p1, p2) ->
              let pc = piece_at b p1 in
              let new_map = PositionMap.remove p1
                (PositionMap.add p2 pc map) in
                  (new_map, flip_side side)
          | Castle Queenside ->
              
          | Castle Kingside ->
end
