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

  (* build position from pair of integers:
   *   create_pos rank file,
   * where rank, file are between 0 and 7, else
   * raises InvalidPosition
   *)
  val create_pos : int -> int -> position

  (* convert pos to tuple of coordinates for engine *) 
  val pos_to_coord : position -> int*int
  
  (* convert from FEN position to position *)
  val fen_to_pos : string -> position option
  
  (* convert from valid FEN to board *)
  val fen_decode : string -> board option
  
  (* convert from board to FEN *)
  val fen_encode : board -> string

  (* which color is to play *)
  val to_play : board -> color

  (* all pieces on current board *)
  val all_pieces : board -> (position * piece) list

  (* all valid moves *)
  val all_moves : board -> move list

  (* returns None if the move is invalid *)
  val play : board -> move -> board option

  (* returns whether current color in check *)
  val check : board -> bool

  (* returns whether current player has lost *)
  val checkmate : board -> bool
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
                       ep_target : position option}
  type board = (piece PositionMap.t) * board_config
  
  let in_bounds rank file =
    (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7)
  
  let create_pos rank file =
    if in_bounds rank file then Pos (rank, file)
    else raise InvalidPosition
      
  let pos_to_coord pos =
    match pos with
      | Pos(rank, file) -> (rank, file)

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
  
  (* Boolean lookup *)  
  let occupied pos board =
    match lookup pos board with
      | Some _ -> true
      | None -> false

  let char_to_piece c =
    let lower_c = Char.lowercase c in
    let name =
      if lower_c = 'p' then Pawn
      else if lower_c = 'n' then Knight
      else if lower_c = 'b' then Bishop
      else if lower_c = 'r' then Rook
      else if lower_c = 'q' then Queen
      else King
    in
      if Char.uppercase c = c then White name
      else Black name
  
  let fen_to_map str =
    let rec fen_to_map_r str map rank file =
      if str = "" || rank < 0 then map
      else
        let c = String.get str 0 in
        let ascii = Char.code c in
        let len = String.length str in
        let tail = String.sub str 1 (len - 1) in
          if c = '/' || file >= 8 then
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
    if str = "-" || String.length str != 2 then None
    else
      let f = String.get str 0 in
      let r = String.get str 1 in
      let file = (Char.code (Char.lowercase f)) - 97 in
      let rank = (Char.code r) - 49 in
        try Some (create_pos rank file)
        with InvalidPosition -> None
  
  let fen_decode str =
    let fen_re_string =
      "^\\(\\([pnbrqk1-8]+/\\)+[pnbrqk1-8]+\\)[ \t]+" ^
      "\\(w\\|b\\)[ \t]+\\([kq]+\\|-\\)[ \t]+\\([a-h][1-8]\\|-\\)$" in
    let fen_re = Str.regexp_case_fold fen_re_string in
      if Str.string_match fen_re str 0 then
        let fen_pcs = Str.matched_group 1 str in
        let fen_color = Str.matched_group 3 str in
        let fen_castle = Str.matched_group 4 str in
        let fen_ep = Str.matched_group 5 str in
        let map = fen_to_map fen_pcs in
        let to_play = fen_to_color fen_color in
        let cas = fen_to_castle fen_castle in
        let ep_target = fen_to_pos fen_ep in
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
                  let new_str = str ^ gap_str ^ c_str in
                    map_to_fen_r new_str rank (file + 1) 0
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

  let same_color pc1 pc2 =
    match (pc1, pc2) with
      | (White _, White _) | (Black _, Black _) -> true
      | (White _, Black _) | (Black _, White _) -> false

  let same_color_dir dir pc2 =
    if dir = 1 then same_color (White King) pc2
    else same_color (Black King) pc2

  let neighbor dr df pos =
    let Pos(r0, f0) = pos in
    let (r1, f1) = (r0 + dr, f0 + df) in
      if in_bounds r1 f1 then Some (Pos(r1, f1)) else None

  let vector pos1 pos2 =
    let (Pos(r1, f1), Pos(r2, f2)) = (pos1, pos2) in
      (r2 - r1, f2 - f1)

  let unit_vector pos1 pos2 =
    let rec gcd a b =
      let a = (if a < b then a else b) in
      let b = (if a < b then b else a) in
      let r = b mod a in
        if r = 0 then a
        else gcd r a
    in
    let (dr, df) = vector pos1 pos2 in
      if dr = 0 && df = 0 then (0, 0)
      else if dr = 0 then (0, df / (abs df))
      else if df = 0 then (dr / (abs dr), 0)
      else let m = gcd (abs dr) (abs df) in (dr / m, df / m)
    

  let rec clear_path occup bd pos1 pos2 =
    let (dr, df) = unit_vector pos1 pos2 in
      match neighbor dr df pos1 with
        | None -> false
        | Some nb ->
            nb = pos2 || (not (occup nb bd) && clear_path occup bd nb pos2)

  let unobstructed = clear_path occupied

  let crawl vectors limit bd pos pc =
    let rec crawl_r cursors squares iter =
      if iter = 0 || cursors = [] then squares
      else
        let update cursors (vec, pos0) =
          match vec with
            | None -> cursors
            | Some (dr, df) ->
                match neighbor dr df pos0 with
                  | None -> cursors
                  | Some nb ->
                      match lookup nb bd with
                        | None -> (Some (dr, df), nb) :: cursors
                        | Some pc2 -> 
                            if same_color pc pc2 then cursors
                            else (None, nb) :: cursors
        in
        let cursors2 = List.fold_left update [] cursors in
        let add_square lst (vec, pos) = pos :: lst in
        let squares2 = List.fold_left add_square squares cursors2 in
          crawl_r cursors2 squares2 (iter - 1)
    in
    let expand vecs (dr, df) = 
      (dr, df) :: (-dr, df) :: (dr, -df) :: (-dr, -df) :: vecs
    in
    let vectors = List.fold_left expand [] vectors in
    let cursors = List.map (fun vec -> (Some vec, pos)) vectors in
      crawl_r cursors [] limit

  let is_valid_pawn bd move dir =
    let (_, {ep_target}) = bd in
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let target = lookup pos2 bd in
      if dr * dir = 1 then
        match target with
          | None -> df = 0 || (abs df = 1 && ep_target = Some pos2)
          | Some pc -> abs df = 1 && not (same_color_dir dir pc)
      else match neighbor dir 0 pos1 with
        | None -> false
        | Some nb ->
            let Pos(rank, _) = pos1 in
              dr * dir = 2 && df = 0 &&
              target = None && not (occupied nb bd) && 
              (if dir = 1 then rank = 1 else rank = 6)

  let direction_of_piece pc =
    match pc with
      | Black _ -> -1
      | White _ -> 1

  let generate_moves_pawn bd pos pc =
    let dir = direction_of_piece pc in
    let targets = [neighbor dir 0 pos; neighbor (dir * 2) 0 pos;
                   neighbor dir 1 pos; neighbor dir (-1) pos]
    in
    let add_weeded r tgt =
      match tgt with
        | None -> r
        | Some tgt -> 
            let mv = (pos, tgt) in
              if is_valid_pawn bd mv dir then tgt :: r else r
    in
      List.fold_left add_weeded [] targets

  let generate_moves_from bd pos pc =
    let targets = match pc with
      | White Pawn | Black Pawn -> generate_moves_pawn bd pos pc
      | White Knight | Black Knight -> crawl [(1, 2); (2, 1)] 1 bd pos pc
      | White Bishop | Black Bishop -> crawl [(1, 1)] 8 bd pos pc
      | White Rook | Black Rook -> crawl [(1, 0); (0, 1)] 8 bd pos pc
      | White Queen | Black Queen -> crawl [(1, 1); (1, 0); (0, 1)] 8 bd pos pc
      | White King | Black King -> crawl [(1, 1); (1, 0); (0, 1)] 1 bd pos pc
    in List.map (fun pos2 -> Standard(pos, pos2)) targets

  let generate_without_castles bd =
    let (map, cfg) = bd in
    let to_play = cfg.to_play in
    let add_moves pos pc moves =
      if same_color pc to_play then
        moves @ (generate_moves_from bd pos pc)
      else moves
    in
      PositionMap.fold add_moves map []

  let in_check pos bd =
    let under_attack prev move =
      match move with
        | Standard (_, pos2) -> (pos2 = pos) || prev
        | Castle _ -> false
    in
    let opponent_moves = generate_without_castles (flip bd) in
      List.fold_left under_attack false opponent_moves

  let rec clear_of_check = clear_path in_check

  let can_castle ctl bd =
    let (_, cfg) = bd in
    let cas = cfg.cas in
    let to_play = cfg.to_play in
      match (to_play, ctl) with
        | (White _, Kingside) ->
            cas.wK && clear_of_check bd (create_pos 0 4) (create_pos 0 6) &&
            not (in_check (create_pos 0 4) bd) &&
            unobstructed bd (create_pos 0 4) (create_pos 0 7)
        | (White _, Queenside) ->
            cas.wQ && clear_of_check bd (create_pos 0 4) (create_pos 0 2) &&
            not (in_check (create_pos 0 4) bd) &&
            unobstructed bd (create_pos 0 0) (create_pos 0 4)
        | (Black _, Kingside) ->
            cas.bK && clear_of_check bd (create_pos 7 4) (create_pos 7 6) &&
            not (in_check (create_pos 7 4) bd) &&
            unobstructed bd (create_pos 7 4) (create_pos 7 7)
        | (Black _, Queenside) ->
            cas.bQ && clear_of_check bd (create_pos 7 4) (create_pos 7 2) &&
            not (in_check (create_pos 7 4) bd) &&
            unobstructed bd (create_pos 7 0) (create_pos 7 4)

  let generate_moves bd =
    let std_moves = generate_without_castles bd in
    let moves =
      if can_castle Kingside bd then
        (Castle Kingside) :: std_moves
      else std_moves
    in
      if can_castle Queenside bd then
        (Castle Queenside) :: moves
      else moves

  (************ helper functions for is_valid ************)

  let is_valid_knight bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let pattern = (dR, dF) = (1, 2) || (dR, dF) = (2, 1) in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color_dir dir pc)

  let is_valid_bishop bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = unit_vector pos1 pos2 in
    let (dR, dF) = (abs dr, abs df) in
    let pattern = dR = dF && unobstructed bd pos1 pos2 in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color_dir dir pc)

  let is_valid_rook bd move dir =
    let (pos1, pos2) = move in
    let (dr, df) = unit_vector pos1 pos2 in
    let pattern =
      (dr = 0 || df = 0) && unobstructed bd pos1 pos2
    in
      match lookup pos2 bd with
        | None -> pattern
        | Some pc -> pattern && not (same_color_dir dir pc)
        
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
      | King -> is_valid_king

  let is_valid bd move =
    match move with
      | Standard (pos1, pos2) ->
	      (match lookup pos1 bd with
	         | None -> false
	         | Some (White pc) ->
	             same_color (White pc) (to_play bd) &&
	             is_valid_for pc bd (pos1, pos2) 1
             | Some (Black pc) ->
                 same_color (Black pc) (to_play bd) &&
                 is_valid_for pc bd (pos1, pos2) (-1)
          )
      | Castle ctl -> can_castle ctl bd

  let determine_target pc pos1 pos2 =
    let dir = direction_of_piece pc in
    let (dr, df) = vector pos1 pos2 in
      if (pc = White Pawn || pc = Black Pawn) && dr * dir = 2
      then neighbor dir 0 pos1
      else None

  let new_permissions pc pos1 cas =
    let {wK; wQ; bK; bQ} = cas in
      match pc with
        | White King -> {wK = false; wQ = false; bK; bQ}
        | Black King -> {wK; wQ; bK = false; bQ = false}
        | White Rook ->
            if pos1 = create_pos 0 0 then {wK; wQ = false; bK; bQ}
            else if pos1 = create_pos 0 7 then {wK = false; wQ; bK; bQ}
            else cas
        | Black Rook ->
            if pos1 = create_pos 7 0 then {wK; wQ; bK; bQ = false}
            else if pos1 = create_pos 7 7 then {wK; wQ; bK = false; bQ}
            else cas
        | _ -> cas

  let is_pawn pc =
    match pc with
      | White Pawn | Black Pawn -> true
      | _ -> false

  let handle_std bd pc pos1 pos2 =
    let (map, {to_play; cas; ep_target}) = bd in
    let new_target = determine_target pc pos1 pos2 in
    let new_cas = new_permissions pc pos1 cas in
    let new_cfg = {to_play; cas = new_cas; ep_target = new_target} in
    let tmp = PositionMap.add pos2 pc map in
    let prelim = PositionMap.remove pos1 tmp in
      if is_pawn pc && ep_target = Some pos2 then
        let (Pos(r1, _), Pos(_, f2)) = (pos1, pos2) in
        let ep_rem = create_pos r1 f2 in
          Some (PositionMap.remove ep_rem prelim, new_cfg)
      else if is_pawn pc then
        let Pos(r2, _) = pos2 in
          match pc with
            | White _ ->
                if r2 = 7 then
                  Some (PositionMap.add pos2 (White Queen) prelim, new_cfg)
                else Some (prelim, new_cfg)
            | Black _ ->
                if r2 = 0 then
                  Some (PositionMap.add pos2 (Black Queen) prelim, new_cfg)
                else Some(prelim, new_cfg)
      else Some (prelim, new_cfg)

  let check bd =
    let (map, _) = bd in
    let king_only = match to_play bd with
      | White _ -> PositionMap.filter (fun k v -> v = White King) map
      | Black _ -> PositionMap.filter (fun k v -> v = Black King) map
    in
    let (king_pos, _) = PositionMap.choose king_only in
      in_check king_pos bd

  let rec exec bd move =
    match (move, to_play bd) with
      | (Standard (pos1, pos2), _) ->
          (match lookup pos1 bd with
            | None -> None
            | Some pc -> handle_std bd pc pos1 pos2)
      | (Castle Queenside, White _) ->
          (match exec bd (Standard(create_pos 0 0, create_pos 0 3)) with
            | None -> None
            | Some new_bd ->
                exec new_bd (Standard(create_pos 0 4, create_pos 0 2)))
      | (Castle Queenside, Black _) ->
          (match exec bd (Standard(create_pos 7 0, create_pos 7 3)) with
            | None -> None
            | Some new_bd ->
                exec new_bd (Standard(create_pos 7 4, create_pos 7 2)))
      | (Castle Kingside, White _) ->
          (match exec bd (Standard(create_pos 0 7, create_pos 0 5)) with
            | None -> None
            | Some new_bd ->
                exec new_bd (Standard(create_pos 0 4, create_pos 0 6)))
      | (Castle Kingside, Black _) ->
          (match exec bd (Standard(create_pos 7 7, create_pos 7 5)) with
            | None -> None
            | Some new_bd ->
                exec new_bd (Standard(create_pos 7 4, create_pos 7 6)))

  let play bd move =
    if is_valid bd move then
      match exec bd move with
        | None -> None
        | Some new_bd -> 
            if not (check new_bd) then Some (flip new_bd)
            else None
    else None

  let all_moves bd =
    List.filter (fun mv -> play bd mv != None) (generate_moves bd)

  let checkmate bd =
    if not (check bd) then false
    else all_moves bd = []
end


module BitBoard : BOARD =
struct
  type bitmask = int64
  type position = bitmask
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  type color = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type board =
  {
    pieces: bitmask array;
    all_pcs : bitmask;
    to_play : bitmask;
    castling: bitmask;
    ep_target: position
  }

  exception InvalidPosition
  
  (**************** bitwise operator notation ****************)
  
  let ($+$) = Int64.add
  let ($*$) = Int64.mul
  let ($-$) = Int64.sub
  let ($/$) = Int64.div
  let ($%$) = Int64.rem
  let ($&$) = Int64.logand
  let ($|$) = Int64.logor
  let ($^$) = Int64.logxor
  let ($>>$) = Int64.shift_right_logical
  let ($<<$) = Int64.shift_left
  
  (************************* castles *************************)
  
  let wKingside = 0x00000000000000F0L
  let wQueenside = 0x000000000000001DL
  let bKingside = 0xF000000000000000L
  let bQueenside = 0x1D00000000000000L
  
  (*********** functions for manipulating bitmasks ***********)
  
  let opponent bd =
    bd.all_pcs $^$ bd.to_play

  let rank_masks =
    let rank_mask i = 0x00000000000000FFL $<<$ (8 * i) in
      Array.init 8 rank_mask

  let file_masks =
    let file_mask i = 0x0101010101010101L $<<$ i in
      Array.init 8 file_mask
  
  let lsb pos = pos $&$ (Int64.neg pos) (* get least significant bit *)

  let f_projection pos = lsb (pos $%$ 0x00000000000000FFL)
  
  let r_projection pos = lsb (pos $/$ (r_projection pos))

  let rank pos = (r_projection pos) $*$ 0x00000000000000FFL
  
  let file pos = (f_projection pos) $*$ 0x0101010101010101L
  
  (* Masks for computing the diagonals *)
  let nw_mask = 0xFF7F3F1F0F070301L
  let se_mask = Int64.lognot nw_mask

  let ne_mask = 0xFEFCF8F0E0C08000L
  let sw_mask = 0x000103070F1F3F7FL
  let bdr_mask = 0x0102040810204080L
  
  let diag_ne pos =
    let mask = if pos $&$ se_mask > 0L then se_mask else nw_mask in
      (diag_proj pos $*$ 0x8040201008040201L) $&$ mask
  
  let diag_nw pos =
    let mask =
      if pos $&$ sw_mask > 0L then sw_mask
      else if pos $&$ bdr_mask > 0L then bdr_mask
      else ne_mask
    in (diag_proj2 pos $*$ 0x8102040810204081L) $&$ mask
    
  (***********************************************************)


  let init_board =
    let init_bits =
      [|
        0x000000000000FF00L;  (* white pawns *)
        0x0000000000000042L;  (* white knights *)
        0x0000000000000024L;  (* white bishops *)
        0x0000000000000081L;  (* white rooks *)
        0x0000000000000008L;  (* white queen *)
        0x0000000000000010L;  (* white king *)
        0x00FF000000000000L;  (* black pawns *)
        0x4200000000000000L;  (* black knights *)
        0x2400000000000000L;  (* black bishops *)
        0x8100000000000000L;  (* black rooks *)
        0x0800000000000000L;  (* black queen *)
        0x1000000000000000L   (* black king *)
      |] in
    let all = Array.fold_left ($|$) 0L init_bits in
    let white = Array.fold_left ($|$) 0L (Array.sub 0 6 init_bits) in
    let cas = wKingside $|$ wQueenside $|$ bKingside $|$ bQueenside
    in  {
          pieces = init_bits;
          all_pcs = all;
          to_play = white;
          castling = cas;
          ep_target = 0L
        }

  let in_bounds rank file =
    (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7)
  
  let create_pos rank file =
    if in_bounds rank file then
      let bit_index = rank * 8 + file in
        1L $<<$ bit_index
    else 0x0L

  let pos_to_coord pos =
    let r = r_projection pos in
    let f = f_projection pos in
    let rank =
      (if r $&$ 0x0101010100000000L != 0L then 4 else 0) +
      (if r $&$ 0x0101000001010000L != 0L then 2 else 0) +
      (if r $&$ 0x0100010001000100L != 0L then 1 else 0) in
    let file =
      (if r $&$ 0xF0 != 0L then 4 else 0) +
      (if r $&$ 0xCC != 0L then 2 else 0) +
      (if r $&$ 0xAA != 0L then 1 else 0)
    in (rank, file)

  let fen_to_pos =
    if str = "-" || String.length str != 2 then None
    else
      let f = String.get str 0 in
      let r = String.get str 1 in
      let file = (Char.code (Char.lowercase f)) - 97 in
      let rank = (Char.code r) - 49 in
        create_pos rank file

  let piece_to_index pc =
    match pc with
      | White Pawn -> 0
      | White Knight -> 1
      | White Bishop -> 2
      | White Rook -> 3
      | White Queen -> 4
      | White King -> 5
      | Black Pawn -> 6
      | Black Knight -> 7
      | Black Bishop -> 8
      | Black Rook -> 9
      | Black Queen -> 10
      | Black King -> 11

  let index_to_piece i =
    try
      Some
        ([|
          White Pawn; White Knight;
          White Bishop; White Rook;
          White Queen; White King;
          Black Pawn; Black Knight;
          Black Bishop; Black Rook;
          Black Queen; Black King;
        |].(i))
    with Invalid_argument _ -> None

  let char_to_piece c =
    let lower_c = Char.lowercase c in
    let name =
      if lower_c = 'p' then Pawn
      else if lower_c = 'n' then Knight
      else if lower_c = 'b' then Bishop
      else if lower_c = 'r' then Rook
      else if lower_c = 'q' then Queen
      else King
    in
      if Char.uppercase c = c then White name
      else Black name
  
  let fen_to_bits str =
    let rec fen_to_bits_r str bits rank file =
      if str = "" || rank < 0 then bits
      else
        let c = String.get str 0 in
        let ascii = Char.code c in
        let len = String.length str in
        let tail = String.sub str 1 (len - 1) in
          if c = '/' || file >= 8 then
            fen_to_map_r tail map (rank - 1) 0
          else if ascii >= 48 && ascii < 58 then
            let gap = ascii - 48 in
              fen_to_map_r tail map rank (file + gap)
          else
            let index = piece_to_index (char_to_piece c) in
            let pos = create_pos rank file in
            let _ = bits.(index) <- bits.(index) $|$ pos in
              fen_to_map_r tail bits rank (file + 1)
    in fen_to_bits_r str (Array.make 12 0L) 7 0

  let fen_to_color str =
    if str = "b" then Black King else White King

  let fen_to_castle str =
    let wK = if String.contains str 'K' then wKingside else 0x0L in
    let wQ = if String.contains str 'Q' then wQueenside else 0x0L in
    let bK = if String.contains str 'k' then bKingside else 0x0L in
    let bQ = if String.contains str 'q' then bQueenside else 0x0L in
      wK $|$ wQ $|$ bK $|$ bQ

  let fen_to_pos str =
    if str = "-" || String.length str != 2 then None
    else
      let f = String.get str 0 in
      let r = String.get str 1 in
      let file = (Char.code (Char.lowercase f)) - 97 in
      let rank = (Char.code r) - 49 in
        create_pos rank file
  
  let fen_decode str =
    let fen_re_string =
      "^\\(\\([pnbrqk1-8]+/\\)+[pnbrqk1-8]+\\)[ \t]+" ^
      "\\(w\\|b\\)[ \t]+\\([kq]+\\|-\\)[ \t]+\\([a-h][1-8]\\|-\\)$" in
    let fen_re = Str.regexp_case_fold fen_re_string in
      if Str.string_match fen_re str 0 then
        let fen_pcs = Str.matched_group 1 str in
        let fen_color = Str.matched_group 3 str in
        let fen_castle = Str.matched_group 4 str in
        let fen_ep = Str.matched_group 5 str in
        let bits = fen_to_bits fen_pcs in
        let all = Array.fold_left ($|$) 0L bits in
        let to_play =
          match fen_to_color fen_color with
            | White _ -> Array.fold_left ($|$) 0L (Array.sub 0 6 bits)
            | Black _ -> Array.fold_left ($|$) 0L (Array.sub 6 6 bits)
        let cas = fen_to_castle fen_castle in
        let ep_target = fen_to_pos fen_ep in
          Some
          {
            pieces = bits;
            all_pcs = all;
            to_play = to_play;
            castling = cas;
            ep_target = ep_target}
      else None

  let occupied bits rank file piece_index =
    let pos = create_pos rank file in
    let bit_index = rank * 8 + file in
    let masked = pos $&$ bits.(piece_index) in
      to_int (masked $>>$ bit_index)

  let bits_to_fen bits =
    let rec bits_to_fen_r str rank file gap =
      let gap_str = if gap > 0 then string_of_int gap else "" in
        if file >= 8 && rank <= 0 then
          str ^ gap_str
        else if file >= 8 && rank > 0 then
          bits_to_fen_r (str ^ gap_str ^ "/") (rank - 1) 0 0
        else
          let occupied_by = occupied bits rank file in
          let ascii =
            0x50 * (occupied_by  0) +   (* 'P' *)
            0x4E * (occupied_by  1) +   (* 'N' *)
            0x42 * (occupied_by  2) +   (* 'B' *)
            0x52 * (occupied_by  3) +   (* 'R' *)
            0x51 * (occupied_by  4) +   (* 'Q' *)
            0x4B * (occupied_by  5) +   (* 'K' *)
            0x70 * (occupied_by  6) +   (* 'p' *)
            0x6E * (occupied_by  7) +   (* 'n' *)
            0x62 * (occupied_by  8) +   (* 'b' *)
            0x72 * (occupied_by  9) +   (* 'r' *)
            0x71 * (occupied_by 10) +   (* 'q' *)
            0x6B * (occupied_by 11)     (* 'k' *)
          in
            if ascii = 0 then bits_to_fen_r str rank (file + 1) (gap + 1)
            else
              let pc_char = Char.chr ascii in
              let new_str = Printf.sprintf "%s%s%c" str gap_str pc_char in
                bits_to_fen_r new_str rank (file + 1) 0
    in bits_to_fen_r "" 7 0 0
    
  let player_to_fen player =
    if player $&$ bd.pieces.(5) = 0 then "b" else "w"
  
  let castle_to_fen cas =
    let str =
      (if cas $&$ wKingside != 0L then "K" else "") ^
      (if cas $&$ wQueenside != 0L then "Q" else "") ^
      (if cas $&$ bKingside != 0L then "k" else "") ^
      (if cas $&$ bQueenside != 0L then "q" else "")
    in if str = "" then "-" else str
  
  
  let target_to_fen pos =
    if pos = 0x0L then "-"
    else
      let rp = r_projection pos in
      let fp = f_projection pos in
      let rank =
        if rp =      0x0000000000000001L then "1"
        else if rp = 0x0000000000000100L then "2"
        else if rp = 0x0000000000010000L then "3"
        else if rp = 0x0000000001000000L then "4"
        else if rp = 0x0000000100000000L then "5"
        else if rp = 0x0000010000000000L then "6"
        else if rp = 0x0001000000000000L then "7"
        else if rp = 0x0100000000000000L then "8"
        else raise InvalidPosition
      in
      let file =
        if fp =      0x01 then "a"
        else if fp = 0x02 then "b"
        else if fp = 0x04 then "c"
        else if fp = 0x08 then "d"
        else if fp = 0x10 then "e"
        else if fp = 0x20 then "f"
        else if fp = 0x40 then "g"
        else if fp = 0x80 then "h"
        else raise InvalidPosition
      in file ^ rank
  
  let fen_encode bd =
    let pcs_fen = bits_to_fen bd in
    let color_fen = player_to_fen bd.to_play in
    let castle_fen = castle_to_fen bd.castling in
    let ep_fen = target_to_fen bd.ep_target in
      pcs_fen ^ " " ^ color_fen ^ " " ^ castle_fen ^ " " ^ ep_fen

  let to_play bd =
    if bd.to_play $&$ bd.pieces.(6) = 0
    then Black King
    else White King

  let piece_at bd rank file =
    let (bits, _) = bd in
    let occupied_by = occupied bits rank file in
    let pass1 = Array.mapi (fun i _ -> (i + 1) * (occupied_by i)) bits in
      index_to_piece ((Array.fold_left (+) 0 bits) - 1)

  let all_pieces bd =
    let all_pieces_r pcs rank file =
      if rank >= 8 then pcs
      else if file >= 8 then all_pieces_r pcs (rank + 1) 0
      else match piece_at bd rank file with
        | None -> all_pieces_r pcs rank (file + 1)
        | Some pc -> all_pieces_r (pc :: pcs) rank (file + 1)

  let pawn_moves bd pos =
    let bits = bd.pieces in
    let (forward, l_mask, r_mask, start_rank) =
      let nlt = Int64.lognot (file_masks.(0)) in
      let nrt = Int64.lognot (file_masks.(7)) in
        match to_play bd with
          | White _ ->
              (($<<$), nlt, nrt, rank_masks.(1))
          | Black _ ->
              (($>>$), nrt, nlt, rank_masks.(6))
    in
    let empty = Int64.lognot bd.all_pcs in
    let opponent = opponent bd in
    let fwd_by_one = (forward pos 8) $&$ empty in
    let virgin = pos $&$ start_rank in
    let fwd_by_two = empty $&$ (forward (empty $&$ (forward virgin 8)) 8) in
    let attack_l = forward (pos $&$ l_mask) 9 in
    let attack_r = forward (pos $&$ r_mask) 7 in
    let targets = opponent $|$ (ep_target bd) in
    let captures = (attack_l $|$ attack_r) $&$ targets in
      fwd_by_one $|$ fwd_by_two $|$ captures

  let knight_moves bd pos =
    let bits = bd.pieces in
    let empty = Int64.lognot bd.all_pcs in
    let opponent = opponent bd in
    let mask =
      if pos $&$ 0x0303030303030303L > 0 then 0x0F0F0F0F0F0F0F0FL
      else if pos $&$ 0x3C3C3C3C3C3C3C3CL  > 0 then 0xFFFFFFFFFFFFFFFFL
      else 0xF0F0F0F0F0F0F0F0L in
    let moves =
      ((pos $*$ 0x0000000000028440L) $|$ 
      ((pos $>>$ 24) $*$ 0x0000000000044280L) $|$
      ((pos $*$ 0x0000000000044280L) $>>$ 24)) $&$ mask
    in
      moves $&$ (opp_pcs $|$ empty)

  let rook_moves bd pos =
    let empty = Int64.lognot bd.all_pcs in
    let opponent = opponent bd in
    let up_right = Int64.neg pos in
    let down_left = pos $-$ 0x1L in
    let r_obstructions = bd.all_pcs $&$ rank pos in
    let f_obstructions = bd.all_pcs $&$ file pos in
    let rR = (lsb (up_right $&$ r_obstructions)) $-$ pos in
    let rL = pos $-$ msb (down_left $&$ r_obstructions) in
    let fU = (lsb (up_right $&$ f_obstructions)) $-$ pos in
    let fD = pos $-$ msb (down_left $&$ f_obstructions) in
      (rR $|$ rL $|$ fU $|$ fD) $&$ (empty $|$ opponent)
  
  let bishop_moves bd pos =
    let empty = Int64.lognot bd.all_pcs in
    let opponent = opponent bd in
    let up_right = Int64.neg pos in
    let down_left = pos $-$ 0x1L in
    let ne_obstructions = bd.all_pcs $&$ diag_ne pos in
    let nw_obstructions = bd.all_pcs $&$ diag_nw pos in
    let neU = (lsb (up_right $&$ ne_obstructions)) $-$ pos in
    let neD = pos $-$ msb (down_left $&$ ne_obstructions) in
    let nwU = (lsb (up_right $&$ nw_obstructions)) $-$ pos in
    let nwD = pos $-$ msb (down_left $&$ nw_obstructions) in
      (neU $|$ neD $|$ nwU $|$ nwD) $&$ (empty $|$ opponent)
  
  let queen_moves bd pos = rook_moves bd pos $|$ bishop_moves bd pos
  
  let king_moves bd pos =
    let bits = bd.pieces in
    let empty = Int64.lognot bd.all_pcs in
    let opponent = opponent bd in
    let mask =
      if pos $&$ 0x0101010101010101L > 0 then 0x0F0F0F0F0F0F0F0FL
      else if pos $&$ 0x7E7E7E7E7E7E7E7EL  > 0 then 0xFFFFFFFFFFFFFFFFL
      else 0xF0F0F0F0F0F0F0F0L in
    let moves =
      ((pos $*$ 0x0000000000000102L) $|$ 
      ((pos $>>$ 16) $*$ 0x000000000028100L) $|$
      ((pos $*$ 0x0000000000028100L) $>>$ 16)) $&$ mask
    in
      moves $&$ (opp_pcs $|$ empty)
  
  let castles =

  let generate_moves bd pos =
  
  let is_valid bd mv =
    let src = mv $&$ bd.to_play in
    let dest = mv $^$ src in
      dest $&$ generate_moves bd src != 0x0L

  let exec bd mv =
    let {pieces; all_pcs; to_play; castling; ep_target} = bd in
    let src = mv $&$ to_play in
    let dest = mv $^$ src in
    let to_play' = to_play $^$ mv in
    let all_pcs' = to_play' $|$ (all_pcs $^$ mv) in
    let to_play'' = all_pcs $^$ to_play' in
    let pieces' = Array.map
      (fun bm -> (bm $&$ mv) $^$ bm $^$
      (if bm $&$ src = 0L then 0L else dest)) pieces
    in
    let castling' = castling $^$ (castling $&$ mv) in
    let pawn = (dest $&$ pieces.(0) $&$ 0xFF000000L) $|$ 
    let ep_target' =
      ((dest $&$ pieces.(0) $&$ 0x00000000FF000000L) $>>$ 8) $|$
      ((dest $&$ pieces.(6) $&$ 0x00FF000000000000L) $<<$ 8)
    in
      {
        pieces = pieces';
        all_pcs = all_pcs';
        to_play = to_play'';
        castling = castling';
        ep_target = ep_target'
      }

  let check bd =
    let king = (bd.pieces.(5) $|$ bd.pieces.(11)) $&$ bd.to_play in
    let enemy_pawns = (bd.pieces.(0) $|$ bd.pieces.(6)) $^$ bd.to_play in
    let enemy_knights = (bd.pieces.(1) $|$ bd.pieces.(7)) $^$ bd.to_play in
    let enemy_bishops = (bd.pieces.(2) $|$ bd.pieces.(8)) $^$ bd.to_play in
    let enemy_rooks = (bd.pieces.(3) $|$ bd.pieces.(9))) $^$ bd.to_play in
    let enemy_queen = (bd.pieces.(4) $|$ bd.pieces.(10)) $^$ bd.to_play in
    let enemy_king = (bd.pieces.(5) $|$ bd.pieces.(11)) $^$ bd.to_play in
      ((knight_moves king $&$ enemy_knights) $|$
      (bishop_moves king $&$ (enemy_bishops $|$ enemy_queen)) $|$
      (rook_moves king $&$ (enemy_rooks $|$ enemy_queen)) $|$
      (king_moves king $&$ enemy_king)) > 0x0L

  let play bd mv =
    if is_valid mv then
      let bd' = exec bd mv in
      if check bd' then None
      else Some bd'
    else None

  
  let all_moves bd =

  let checkmate bd = check bd && 
end


module StdBoard : BOARD = MapBoard
