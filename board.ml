open Util

module type BOARD = 
sig
  type position
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  (* encode Black as Black King, White as White King *)
  type side = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type board
  exception InvalidPosition

  (* build position from pair of integers
   * (i.e.,
   *       create_pos rank file,
   * where rank, file are between 0 and 7)
   * raise InvalidPosition if invalid coordinates *)
  val create_pos : int -> int -> position

  (* functions for manipulating positions *)
  val neighbor : int -> int -> position -> position option
  val vector : position -> position -> (int * int)

  (* standard starting board *)
  val init_board : board

  (* which color is to play *)
  val to_play : board -> side

  (* all pieces on current board *)
  val all_pieces : board -> (position, piece) list

  (* is move valid? *)
  val is_valid : board -> move -> bool

  (* all valid moves *)
  val all_moves : board -> move list

  (* what piece is at given position *)
  val lookup : position -> board -> piece option

  (* should return None if the move is invalid *)
  val play : board -> move -> board option

  (* returns color in check or None *)
  val check : board -> side option

  (* returns losing color or None *)
  val checkmate: board -> side option

  (* REQUIRES SIDE AND RIGHT OR LEFT *)
  val can_castle: board -> side -> castle -> bool
  
  val same_side: piece -> piece -> bool
end



module MapBoard : BOARD =
struct
  type position = Pos of int * int
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  type side = piece
  type castle = Queenside of side | Kingside of side
  type move = Standard of position * position | Castle of castle

  module PositionMap = Map.Make(struct
      type t = position
      let compare pos1 pos2 =
        let (Pos(r1, f1), Pos(r2, f2)) = (pos1, pos2)  in
          if r1 < r2 then -1
          else if r1 > r2 then 1
          else if r1 = r2 then
            if f2 < f2 then -1
            else if f1 > f2 then 1
            else 0
    end)

  (* a board is a map of positions to pieces together with extra data *)
  type board_config = {toPlay : side;}
  type board = (piece PositionMap.t) * board_config 
  
  let create_pos rank file : position =
    if (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7) then
      Pos (x, y)
    else raise InvalidPosition

  let neighbor dr df pos : position option =
    let Pos(r0, f0) = pos in
    let (r1, f1) = (r0 + dr, f0 + df) in
    if (r1 >= 0 && r1 <= 7) && (f1 >= 0 && f1 <= 7) then
      Some Pos (r1, f1)
    else
      None

  let vector pos1 pos2 =
    let (Pos(rank1, file1), Pos(rank2, file2)) = (pos1, pos2) in
      (rank2 - rank1, file2 - file1)

  let init_board = 
    let files = [0; 1; 2; 3; 4; 5; 6; 7] in
    let names = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
    let pc_files = List.combine files names in
    let init_pcs = List.fold_left
      (fun r (x, pc) -> (create_pos 7 x, Black pc) :: 
      (create_pos 0 x, White pc) :: r) [] pc_files in
    let init_pawns = List.fold_left 
      (fun r x -> (create_pos 6 x, Black Pawn) ::
      (create_pos 1 x, White Pawn) :: r) [] files in
    let init_bindings = init_pcs @ init_pawns in
    let add_binding board (pos, pc) =
      PositionMap.add pos pc board
    in
      (List.fold_left add_binding PositionMap.empty init_bindings,
        {toPlay = White King})

  (* helper function for exchanging turns *)
  let flip (b: board) =
    let (map, cfg) = b in
      match cfg.toPlay with
        | White x -> (map, {toPlay = Black x})
        | Black x -> (map, {toPlay = White x})

  let to_play b =
    let (_, cfg) = b in cfg.toPlay

  let all_pieces b =
    let (map, cfg) = b in
      PartitionMap.bindings map

  (* helper functions for is_valid *)
  let is_valid_for pc = 
    match pc with 
      | Pawn -> is_valid_pawn
      | Knight -> is_valid_knight
      | Bishop -> is_valid_bishop
      | Rook -> is_valid_rook
      | Queen -> is_valid_queen
      | King -> is_valid_queen

  let is_valid_pawn b mv dir =
    let (pos1, pos2) = mv in
    let (dr, df) = vector pos1 pos2 in
    if dr != dir then false
    else 
    

  let is_valid b mv =
    match mv with
      | Standard (pos1, pos2) ->
          let pc = piece_at b pos1 in
	    match pc with
	      | White pc -> is_valid_for pc b mv 1
	      | Black pc -> is_valid_for pc b mv -1
      | Castle Queenside White _ -> 
      | Castle Queenside Black _ ->
      | Castle Kingside White _ ->
      | Castle Kingside Black _ ->
  
  let lookup pos_opt b =
    
    

  let play b mv =
    let exec map mv =
      match mv with
        | Standard (p1, p2) ->
            let pc = piece_at b p1 in
            let new_map = PositionMap.remove p1
              (PositionMap.add p2 pc map) in
              new_map
        | Castle Queenside White _ ->
            exec (exec map (Standard (create_pos 0 0, create_pos 0 3)))
              (Standard (create_pos 0 4, create_pos 0 2))
        | Castle Queenside Black _ ->
            exec (exec map (Standard (create_pos 7 0, create_pos 7 3)))
              (Standard (create_pos 7 4, create_pos 7 2))
        | Castle Kingside White _ ->
            exec (exec map (Standard (create_pos 0 7, create_pos 0 5)))
              (Standard (create_pos 0 4, create_pos 0 6))
        | Castle Kingside Black _ ->
            exec (exec map (Standard (create_pos 7 7, create_pos 7 5)))
              (Standard (create_pos 7 4, create_pos 7 6))
    in
      if is_valid b mv then 
        let (map, cfg) = b in
          flip (exec map mv, cfg)
      else b
end
