module type BOARD = 
sig
  (* a1 is (0, 0); h8 is (7, 7) *)
  type position
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  (* encode Black as Black King, White as White King *)
  type side = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type board
    (* add the not valid position exception *)

  (* build position *)
  val create_pos : (int * int) -> Pos
  (* standard starting board *)
  val init_board : board
  (* which color is to play *)
  val to_play : board -> side
  (* all positions on current board *)
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
      let compare p1 p2 =
        let (p1, p2) = ((p1r, p1f), (p2r, p2f)) in
          if p1r < p2r then -1
          else if p1r > p2r then 1
          else if p1r = p2r then
            if p1f < p2f then -1
            else if p1f > p2f then 1
            else 0
    end)

  (* a board is a map of location => piece together with extra data *)
  type board_config = {toPlay : side;}
  type board = (piece PositionMap.t) * board_config 
  
  let create_pos (x:int) (y:int) : position =
    if (x>=0&&x<=7) && (y>=0&&y<=7) then Pos (x,y) 
    else raise Exception "Not a valid position"
  let init_board = 
    let files = [0; 1; 2; 3; 4; 5; 6; 7] in
    let names = [Rook; Knight; Bishop; Queen; King; Bishop; Knight; Rook] in
    let pc_files = List.combine files names in
    let init_pcs = List.fold_left
      (fun r (x, pc) -> ((7, x), Black pc) :: ((0, x), White pc) :: r) [] pc_files in
    let init_pawns = List.fold_left 
      (fun r x -> ((6, x), Black Pawn) :: ((1, x), White Pawn) :: r) [] files in
    let init_bindings = init_pcs @ init_pawns in
    let add_binding board ((x, y), pc) =
      PositionMap.add (x, y) pc board
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

  let piece2fn p = 
    match p with 
      | Pawn -> is_valid_pawn
      | Knight -> is_valid_knight
      | Bishop -> is_valid_bishop
      | ...

  let is_valid b mv =
    match mv with
      | Standard (p1, p2) ->
	  let pc = piece_at b p1 in
	    match pc with
	      | White p -> (piece2fn p) b mv 1
	      | Black p -> (piece2fn p) b mv -1
	  
          let pc = piece_at b p1 in
            (match pc with
               | White Pawn -> is_valid_pawn b mv 1
               | Black Pawn -> is_valid_pawn b mv -1
               | White Knight -> is_valid_knight b mv 1
               | Black Knight -> is_valid_knight b mv -1
               | White Bishop -> is_valid_bishop b mv 1
               | Black Bishop -> is_valid_bishop b mv -1
               | White Rook -> is_valid_rook b mv 1
               | Black Rook -> is_valid_rook b mv -1
               | White Queen -> is_valid_queen b mv 1
               | Black Queen -> is_valid_queen b mv -1
               | White King -> is_valid_king b mv 1
               | Black King -> is_valid_king b mv -1
            )
      | Castle Queenside White _ -> 
      | Castle Queenside Black _ ->
      | Castle Kingside White _ ->
      | Castle Kingside Black _ ->
    

  let play b mv =
    let exec map mv =
      match mv with
        | Standard (p1, p2) ->
            let pc = piece_at b p1 in
            let new_map = PositionMap.remove p1
              (PositionMap.add p2 pc map) in
              new_map
        | Castle Queenside White _ ->
            exec (exec map (Standard ((0, 0), (0, 3))))
              (Standard ((0, 4), (0, 2)))
        | Castle Queenside Black _ ->
            exec (exec map (Standard ((7, 0), (7, 3))))
              (Standard ((7, 4), (7, 2)))
        | Castle Kingside White _ ->
            exec (exec map (Standard ((0, 7), (0, 5))))
              (Standard ((0, 4), (0, 6)))
        | Castle Kingside Black _ ->
            exec (exec map (Standard ((7, 7), (7, 5))))
              (Standard ((7, 4), (7, 6)))
    in
      if is_valid b mv then 
        let (map, cfg) = b in
          flip (exec map mv, cfg)
      else b
end
