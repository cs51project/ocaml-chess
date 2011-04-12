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
  val all_pieces : board -> (position * piece) list

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
  exception InvalidPosition

  module PositionMap = Map.Make(struct
      type t = position
      let compare (Pos(r1, f1)) (Pos(r2, f2)) =
        if r1 < r2 then -1
        else if r1 > r2 then 1
        else if r1 = r2 && f1 < f2 then -1
        else if r1 = r2 && f1 > f2 then 1
        else 0
    end)

  (* a board is a map of positions to pieces together with extra data *)
  type castle_rec = {wK : bool; wQ : bool; bK : bool; bQ : bool}
  type board_config = {to_play : color; cas: castle_rec;
                       ep_target : position option;}
  type board = (piece PositionMap.t) * board_config 
  
  let in_bounds rank file =
    (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7)
  
  let create_pos rank file =
    if in_bounds rank file then Pos (rank, file)
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
        Some (PositionMap.find pos map)
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
      else King
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
          if c = '/' then
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

  let fen_to_pos str =
    let f = String.get str 0 in
    let r = String.get str 1 in
    let file = (Char.code (Char.lowercase f)) - 97 in
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
        let ep_target =
          try Some (fen_to_pos fen_ep)
          with InvalidPosition -> None
        in
          Some (map, {to_play = to_play; cas = cas; ep_target = ep_target})
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
    let rec map_to_fen_r str rank file gap =
      let gap_str = if gap > 0 then string_of_int gap else "" in
        if file >= 8 && rank <= 0 then
          str ^ gap_str
        else if file >= 8 && rank > 0 then
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
    in map_to_fen_r "" 7 0 0
  
  let color_to_fen player =
    match player with
      | White _ -> "w"
      | Black _ -> "b"
  
  let castle_to_fen cas =
    let {wK; wQ; bK; bQ} = cas in
    let str =
      (if wK then "K" else "") ^
      (if wQ then "Q" else "") ^
      (if bK then "k" else "") ^
      (if bQ then "q" else "")
    in if str = "" then "-" else str
  
  
  let target_to_fen pos =
    match pos with
      | None -> "-"
      | Some pos ->
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
  let flip bd =
    let (map, cfg) = bd in
    let {to_play; cas; ep_target} = cfg in
      match to_play with
        | White x -> (map, {to_play = Black x; cas; ep_target})
        | Black x -> (map, {to_play = White x; cas; ep_target})

  let to_play bd =
    let (_, cfg) = bd in cfg.to_play

  let all_pieces bd =
    let (map, cfg) = bd in
      PositionMap.bindings map

  (* NEED TO IMPLEMENT THESE!!!! *)
  let generate_moves bd = []
  let all_moves bd = []

  (************ helper functions for is_valid ************)
  
  let same_color dir pc2 =
    match (dir = 1, pc2) with
      | (true, White _) | (false, Black _) -> true
      | (true, Black _) | (false, White _) -> false

  let neighbor dr df pos =
    let Pos(r0, f0) = pos in
    let (r1, f1) = (r0 + dr, f0 + df) in
      if in_bounds r1 f1 then Some (Pos(r1, f1)) else None

  let vector pos1 pos2 =
    let (Pos(r1, f1), Pos(r2, f2)) = (pos1, pos2) in
      (r2 - r1, f2 - f1)

  (* returns whether pos is in check by current player *)
  let in_check pos bd =
    let under_attack prev move =
      let (_, pos2) = move in
        pos2 = pos || prev
    in
      List.fold_left under_attack false (generate_moves bd)

  let rec clear_path occup bd pos1 pos2 =
    let (dr, df) = vector pos1 pos2 in
      match neighbor dr df pos1 with
        | None -> false
        | Some nb ->
            nb = pos2 || (not (occup nb bd) && clear_path occup bd nb pos2)

  let rec unobstructed = clear_path occupied
  let rec clear_of_check = clear_path in_check

  (* Returns bool indicating whether given castle is currently
   * allowed by color to play.
   *)
  let can_castle ctl bd =
    let (_, cfg) = bd in
    let cas = cfg.cas in
    let to_play = cfg.to_play in
    let flipped = flip bd in
      match (to_play, ctl) with
        | (White _, Kingside) ->
            cas.wK && clear_of_check flipped
              (create_pos 0 4) (create_pos 0 6) &&
            unobstructed bd (create_pos 0 4) (create_pos 0 7)
        | (White _, Queenside) ->
            cas.wQ && clear_of_check flipped
              (create_pos 0 4) (create_pos 0 2) &&
            unobstructed bd (create_pos 0 0) (create_pos 0 4)
        | (Black _, Kingside) ->
            cas.bK && clear_of_check flipped
              (create_pos 7 4) (create_pos 7 6) &&
            unobstructed bd (create_pos 7 4) (create_pos 7 7)
        | (Black _, Queenside) ->
            cas.bQ && clear_of_check flipped
              (create_pos 7 4) (create_pos 7 2) &&
            unobstructed bd (create_pos 7 0) (create_pos 7 4)

  (* Does this leave king in check? *)
  let is_valid_pawn bd move dir =
    let (_, {ep_target}) = bd in
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let target = lookup pos2 bd in
      if dr * dir = 1 then
        match target with
          | None -> df = 0 || (abs df = 1 && ep_target = Some pos2)
          | Some pc -> abs df = 1 && not (same_color dir pc)
      else match neighbor dir 0 pos1 with
        | None -> false
        | Some nb ->
            let Pos(rank, _) = pos1 in
              dr * dir = 2 && df = 0 &&
              target = None && not (occupied nb bd) && 
              (if dir = 1 then rank = 1 else rank = 6)

  let is_valid_knight bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let pattern = (dR, dF) = (1, 2) || (dR, dF) = (2, 1) in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color dir pc)

  let is_valid_bishop bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let pattern = (dR, dF) = (1, 1) && unobstructed bd pos1 pos2 in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color dir pc)

  let is_valid_rook bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let pattern =
      (dR, dF) = (1, 0) || (dR, dF) = (0, 1) &&
      unobstructed bd pos1 pos2
    in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color dir pc)
        
  let is_valid_queen bd move dir =
    is_valid_rook bd move dir ||
    is_valid_bishop bd move dir

  let is_valid_king bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let dist = max dR dF in
      dist = 1 && is_valid_queen bd move dir

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
	      (match lookup pos1 bd with
	         | None -> false
	         | Some (White pc) -> is_valid_for pc bd (pos1, pos2) 1
             | Some (Black pc) -> is_valid_for pc bd (pos1, pos2) (-1)
          )
      | Castle ctl -> can_castle ctl bd

  let play bd move =
    let rec exec map move =
      match (move, to_play bd) with
        | (Standard (pos1, pos2), _) ->
            (match lookup pos1 bd with
               | None -> None
               | Some pc ->
                   let tmp = PositionMap.add pos2 pc map in
                     Some (PositionMap.remove pos1 tmp)
            )
        | (Castle Queenside, White _) ->
            (match exec map (Standard(create_pos 0 0, create_pos 0 3)) with
               | None -> None
               | Some new_map ->
                   exec new_map (Standard(create_pos 0 4, create_pos 0 2))
            )
        | (Castle Queenside, Black _) ->
            (match exec map (Standard(create_pos 7 0, create_pos 7 3)) with
               | None -> None
               | Some new_map ->
                   exec new_map (Standard(create_pos 7 4, create_pos 7 2))
            )
        | (Castle Kingside, White _) ->
            (match exec map (Standard(create_pos 0 7, create_pos 0 5)) with
               | None -> None
               | Some new_map ->
                   exec new_map (Standard(create_pos 0 4, create_pos 0 6))
            )
        | (Castle Kingside, Black _) ->
            (match exec map (Standard(create_pos 7 7, create_pos 7 5)) with
               | None -> None
               | Some new_map ->
                   exec new_map (Standard(create_pos 7 4, create_pos 7 6))
            )
    in
      if is_valid bd move then
        let (map, cfg) = bd in
          match exec map move with
            | None -> None
            | Some new_map -> 
                Some (flip (new_map, cfg))
      else None
end

module StdBoard : BOARD = MapBoard