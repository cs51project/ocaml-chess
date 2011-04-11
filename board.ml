open Util

module type BOARD = 
sig
  type position
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  (* encode Black as Black King, White as White King *)
  type color = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type board
  exception InvalidPosition


  (* standard starting board *)
  val init_board : board
  
  (* convert from valid FEN to board *)
  val fen_decode : string -> board option
  
  (* convert from board to FEN *)
  val fen_encode : board -> string
  
  (* build position from pair of integers:
   *   create_pos rank file,
   * where rank, file are between 0 and 7, else
   * raises InvalidPosition
   *)
  val create_pos : int -> int -> position

  (* which color is to play *)
  val to_play : board -> color

  (* all pieces on current board *)
  val all_pieces : board -> (position, piece) list

  (* all valid moves *)
  val all_moves : board -> move list

  (* returns None if the move is invalid *)
  val play : board -> move -> board option

  (* returns color in check or None *)
  val check : board -> color option

  (* returns losing color or None *)
  val checkmate : board -> color option
end



module MapBoard : BOARD =
struct
  type position = Pos of int * int
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  type color = piece
  type castle = Queenside | Kingside
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
  type castle_rec = {wK : bool; wQ : bool; bK : bool; bQ : bool}
  type board_config = {to_play : color; cas: castle_rec;
                       ep_target : position option;}
  type board = (piece PositionMap.t) * board_config 
  
  let create_pos rank file : position =
    if (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7) then
      Pos (x, y)
    else raise InvalidPosition

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
    let add_binding board (pos, pc) = PositionMap.add pos pc board in
    let cas = {wK = true; wQ = true; bK = true; bQ = true} in
      (List.fold_left add_binding PositionMap.empty init_bindings,
        {to_play = White King; ep_target = None; cas = cas})


  (* Piece in given position *)
  let lookup pos bd =
    let (map, _) = bd in 
      try
        Some (PositionMap.find pos bd)
      with Not_found -> None
    
  let occupied pos board =
    match lookup pos board with
      | Some _ -> true
      | None -> false

  let char_to_piece c =
    let name =
      if c = 'p' then Pawn
      else if c = 'n' then Knight
      else if c = 'b' then Bishop
      else if c = 'r' then Rook
      else if c = 'q' then Queen
      else if c = 'k' then King
    in
      if Char.uppercase c = c then White name
      else Black name
  
  let fen_to_map str =
    let rec fen_to_map_r str map rank file =
      if str = "" then map
      else
        let c = String.get str 0 in
        let ascii = Char.code c in
        let len = String.length str in
        let tail = String.sub str 1 (len - 1) in
          if c = "/" then
            fen_to_map_r tail map (rank - 1) 0
          else if ascii >= 48 && ascii < 58 then
            let gap = ascii - 48 in
              fen_to_map_r tail map rank (file + gap)
          else
            let piece = char_to_piece c in
            let pos = create_pos rank file in
            let new_map = PositionMap.add pos piece map in
              fen_to_map_r tail new_map rank (file + 1)
    in fen_to_map_r str PositionMap.empty 7 0

  let fen_to_color str =
    if str = "b" then Black King
    else White King

  let fen_to_castle str =
    let wK = String.contains str 'K' in
    let wQ = String.contains str 'Q' in
    let bK = String.contains str 'k' in
    let bQ = String.contains str 'q' in
      {wK = wK; wQ = wQ; bK = bK; bQ = bQ}

  let fen_to_target str =
    let f = String.get str 0 in
    let r = String.get str 1 in
    let file = (Char.code (lowercase f)) - 97 in
    let rank = (Char.code r) - 49 in
      create_pos rank file
  
  let fen_decode str =
    let fen_re_string =
      "^\\(([pnbrqk0-8]+/){7}[pnbrqk0-8]+\\)[ \t]+" ^
      "\\(w|b\\)[ \t]+\\([kq]+|-\\)[ \t]+\\([a-h][1-8]\\)$" in
    let fen_re = Str.regexp_case_fold fen_re_string in
      if Str.string_match fen_re str 0 then
        let fen_pcs = Str.matched_group 1 str in
        let fen_color = Str.matched_group 2 str in
        let fen_castle = Str.matched_group 3 str in
        let fen_ep = Str.matched_group 4 str in
        let map = fen_to_map fen_pcs in
        let to_play = fen_to_color fen_color in
        let cas = fen_to_castle fen_castle in
        let ep_target = fen_to_target fen_ep in
          Some (map, {to_play = to_play; cas = cas; ep_target = ep_target;))
      else None


  let piece_to_char pc =
    let (case, name) = match pc with
      | Black pc -> (Char.lowercase, pc)
      | White pc -> (Char.uppercase, pc)
    in
    let letter = match name with
      | Pawn -> 'p'
      | Knight -> 'n'
      | Bishop -> 'b'
      | Rook -> 'r'
      | Queen -> 'q'
      | King -> 'k'
    in case letter

  let map_to_fen bd =
    let map_to_fen_r str rank file gap =
      let gap_str = if gap > 0 then int_to_string gap else "" in
        if file >= 8 && rank <= 0 then
          str ^ gap_str
        else if file >= 8 && rank > 0 then
          let gap = int_to_string gap in
            map_to_fen_r (str ^ gap_str ^ "/") (rank - 1) 0 0
        else
          let pos = create_pos rank file in
            match lookup pos bd with
              | None ->
                  map_to_fen_r str rank (file + 1) (gap + 1)
              | Some pc ->
                  let c = piece_to_char pc in
                  let c_str = Char.escaped c in
                    str ^ gap_str ^ c_str
              
    
  
  let color_to_fen player =
    match player with
      | White _ -> "w"
      | Black -> "b"
  
  let castle_to_fen cas =
    let {wK; wQ; bK; bQ} = cas in
    let str =
      (if wK then "K" else "") ^
      (if wQ then "Q" else "") ^
      (if bK then "k" else "") ^
      (if bQ then "q" else "")
    in if str = "" then "-" else str
  
  
  let target_to_fen pos =
    let Pos(rank, file) = pos in
    let r = Char.chr (rank + 49) in
    let f = Char.chr (file + 97) in
      (Char.escaped f) ^ (Char.escaped r)
  
  let fen_encode bd =
    let (_, cfg) = bd in
    let map_fen = map_to_fen bd in
    let color_fen = color_to_fen cfg.to_play in
    let castle_fen = castle_to_fen cfg.cas in
    let ep_fen = target_to_fen cfg.ep_target in
      map_fen ^ " " ^ color_fen ^ " " ^ castle_fen ^ " " ^ ep_fen

  (* helper function for exchanging turns *)
  let flip (b: board) =
    let (map, cfg) = b in
    let {to_play; cas; ep_target} = cfg in
      match to_play with
        | White x -> (map, {to_play = Black x; cas; ep_target})
        | Black x -> (map, {to_play = White x; cas; ep_target})

  let to_play bd =
    let (_, cfg) = bd in cfg.to_play

  let all_pieces bd =
    let (map, cfg) = bd in
      PartitionMap.bindings map

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

  (* Returns bool indicating whether given castle is currently
   * allowed by color to play.
   *)
  let can_castle =

  (* helper functions for is_valid *)

  (* Look if this causes king to be in check!!!!!!! *)
  let is_valid_pawn bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let Pos(rank, file) = pos1 in 
	  (* Move up one square *)
      if dr*dir = 1 && df = 0 && occupied pos2 board then true
	  (* Move up two squares *)	
      else if dr*dir = 2 && df = 0 && occupied pos2 board
        && occupied Pos(rank+1, file) bd then true
	  (* Take left or right *)
	  else if dr*dir = 1 && df = 1 || df = -1 && (* figure out if there's a piece to take of the opposite color *)
	  (* En-passant *)
	   else if dr*dir = 1 && df = 1 || df = -1 && (* figure out if there's a piece to take of the opposite color *)

  let is_valid_for pc = 
    match pc with 
      | Pawn -> is_valid_pawn
      | Knight -> is_valid_knight
      | Bishop -> is_valid_bishop
      | Rook -> is_valid_rook
      | Queen -> is_valid_queen 
      | King -> is_valid_queen

  let is_valid bd move =
    match move with
      | Standard (pos1, pos2) ->
          let pc = piece_at bd pos1 in
	    match pc with
	      | White pc -> is_valid_for pc bd move 1
	      | Black pc -> is_valid_for pc bd move -1
      | Castle Kingside -> 
      | Castle Queenside ->

  let play bd move =
    let exec map move =
      match (move, to_play bd) with
        | (Standard (p1, p2), _) ->
            let pc = piece_at bd p1 in
            let new_map = PositionMap.remove p1 (PositionMap.add p2 pc map) in
              new_map
        | (Castle Queenside, White _) ->
            exec (exec map (Standard (create_pos 0 0, create_pos 0 3)))
              (Standard (create_pos 0 4, create_pos 0 2))
        | (Castle Queenside, Black _) ->
            exec (exec map (Standard (create_pos 7 0, create_pos 7 3)))
              (Standard (create_pos 7 4, create_pos 7 2))
        | (Castle Kingside, White _) ->
            exec (exec map (Standard (create_pos 0 7, create_pos 0 5)))
              (Standard (create_pos 0 4, create_pos 0 6))
        | (Castle Kingside, Black _) ->
            exec (exec map (Standard (create_pos 7 7, create_pos 7 5)))
              (Standard (create_pos 7 4, create_pos 7 6))
    in
      if is_valid bd move then 
        let (map, cfg) = bd in
          flip (exec map move, cfg)
      else bd
end

module StdBoard : BOARD = MapBoard