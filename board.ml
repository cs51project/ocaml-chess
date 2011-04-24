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
      if dr = 0 and df = 0 then (0, 0)
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
        | Standard (_, pos2) -> pos2 = pos || prev
        | Castle _ -> false
    in
      List.fold_left under_attack false (generate_without_castles (flip bd))

  let rec clear_of_check = clear_path in_check

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
      dr = 0 || df = 0 && unobstructed bd pos1 pos2
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

  let handle_std bd pc pos1 pos2 =
    let (map, {to_play; cas; ep_target}) = bd in
    let new_target = determine_target pc pos1 pos2 in
    let new_cas = new_permissions pc pos1 cas in
    let new_cfg = {to_play; cas = new_cas; ep_target = new_target} in
    let tmp = PositionMap.add pos2 pc map in
    let prelim = PositionMap.remove pos1 tmp in
      if ep_target = Some pos2 then
        let (Pos(r1, _), Pos(_, f2)) = (pos1, pos2) in
        let ep_rem = create_pos r1 f2 in
          Some (PositionMap.remove ep_rem prelim, new_cfg)
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
(*
module BitBoard : BOARD =
struct
  type bitmask = int64
  type position = bitmask
  type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
  type piece = Black of piece_type | White of piece_type
  type color = piece
  type castle = Queenside | Kingside
  type move = Standard of position * position | Castle of castle
  type castle_rec = {wK : bool; wQ : bool; bK : bool; bQ : bool}
  type board_config = {to_play : color; cas: castle_rec;
                       ep_target : position option}
  type board = (bitmask array) * board_config
  exception InvalidPosition
  
  let init_board =
    let init_bits =
      [|
        0x000000000000ff00; 0x00ff000000000000; (* pawns *)
        0x0000000000000042; 0x4200000000000000; (* knights *)
        0x0000000000000024; 0x2400000000000000; (* bishops *)
        0x0000000000000081; 0x8100000000000000; (* rooks *)
        0x0000000000000010; 0x1000000000000000; (* queens *)
        0x0000000000000008; 0x0800000000000000
      |] in
    let cas = {wK = true; wQ = true; bK = true; bQ = true} in
      (init_bits, {to_play = White King; ep_target = None; cas = cas})

  let in_bounds rank file =
    (rank >= 0 && rank <= 7) && (file >= 0 && file <= 7)
  
  let create_pos rank file =
    if in_bounds rank file then
      let bit_index = rank * 8 + file in
        Int64.shift_left 1L bit_index
    else raise InvalidPosition

  let fen_to_pos =
    if str = "-" || String.length str != 2 then None
    else
      let f = String.get str 0 in
      let r = String.get str 1 in
      let file = (Char.code (Char.lowercase f)) - 97 in
      let rank = (Char.code r) - 49 in
        try Some (create_pos rank file)
        with InvalidPosition -> None

  let piece_to_index pc =
    match pc with
      | White Pawn -> 0
      | Black Pawn -> 1
      | White Knight -> 2
      | Black Knight -> 3
      | White Bishop -> 4
      | Black Bishop -> 5
      | White Rook -> 6
      | Black Rook -> 7
      | White Queen -> 8
      | Black Queen -> 9
      | White King -> 10
      | Black King -> 11

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
            let _ = bits.(index) <- Int64.logor bits.(index) pos in
              fen_to_map_r tail bits rank (file + 1)
    in fen_to_bits_r str (Array.make 12 0L) 7 0

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
        let bits = fen_to_bits fen_pcs in
        let to_play = fen_to_color fen_color in
        let cas = fen_to_castle fen_castle in
        let ep_target = fen_to_pos fen_ep in
          Some (bits, {to_play = to_play; cas = cas; ep_target = ep_target})
      else None

  let index_to_ascii i =
    (land i 0) * 

  let bits_to_fen bits =
    let rec bits_to_fen_r str rank file gap =
      let gap_str = if gap > 0 then string_of_int gap else "" in
        if file >= 8 && rank <= 0 then
          str ^ gap_str
        else if file >= 8 && rank > 0 then
          bits_to_fen_r (str ^ gap_str ^ "/") (rank - 1) 0 0
        else
          let bit_index = rank * 8 + file in
          let pos = create_pos rank file in
          let field_to_ascii i field =
            let bit = Int64.shift_right (Int64.logand pos field) bit_index in
            let ascii = (Int64.to_int bit) * (index_to_ascii i)
          let ascii = Array.fold_left (fun a f -> a + field_to_ascii f) 0 bits in
          let c_str = Char.escaped (Char.chr ascii)
          let new_str = str ^ gap_str ^ c_str in
            bits_to_fen_r new_str rank (file + 1) 0
    in bits_to_fen_r "" 7 0 0
    
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
    let pcs_fen = bits_to_fen bd in
    let color_fen = color_to_fen cfg.to_play in
    let castle_fen = castle_to_fen cfg.cas in
    let ep_fen = target_to_fen cfg.ep_target in
      pcs_fen ^ " " ^ color_fen ^ " " ^ castle_fen ^ " " ^ ep_fen

  let to_play bd = (snd bd).to_play

  let all_pieces bd =

  let all_moves bd =

  let play bd mv =

  let check bd =

  let checkmate bd =
end
*)
module StdBoard : BOARD = MapBoard
