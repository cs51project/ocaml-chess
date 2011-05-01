open Board
open Brain

module Order =
struct
  type order = Less | Equal | Greater
end

module type EVAL =
sig
  type board
  type value
  type evaluator
  val ubound : value
  val lbound : value
  val comp : value -> value -> Order.order
  val negate : value -> value
  val init_eval : evaluator (* standard evaluator *)
  val apply : evaluator -> board -> value
  val train : evaluator -> board -> value -> evaluator (* learning function *)
end

module type ENGPARAMS =
sig
  val depth : int
end

module type ENGINE =
sig
  type board
  type move
  type evaluator
  val strat : evaluator -> board -> move option
end

module SimpleEval (B : BOARD) : (EVAL with type board = B.board) =
struct
  type board = B.board
  type value = Finite of int | Inf | NInf
  type evaluator = board -> value
  let ubound = Inf
  let lbound = NInf
  
 (* Tables for evaluation lookups *)
  let pawn_table =
  [|
    [| 0;  0;   0;   0;   0;   0;   0;  0|]; 
    [| 5; 10;  10; -25; -25;  10;  10;  5|];
    [| 5; -5; -10;   0;   0; -10;  -5;  5|];
    [| 0;  0;   0;  25;  25;   0;   0;  0|];
    [| 5;  5;  10;  27;  27;  10;   5;  5|];
    [|10; 10;  20;  30;  30;  20;  10; 10|];
    [|50; 50;  50;  50;  50;  50;  50; 50|];
    [| 0;  0;   0;   0;   0;   0;   0;  0|]
  |]

  let knight_table =
  [|
    [|-50; -40; -20; -30; -30; -20; -40; -50|]; 
    [|-40; -20;   0;   5;   5;   0; -20; -40|];
    [|-30;   5;  10;  15;  15;  10;   5; -30|];
    [|-30;   0;  15;  20;  20;  15;   0; -30|];
    [|-30;   5;  15;  20;  20;  15;   5; -30|];
    [|-30;   0;  10;  15;  15;  10;   0; -30|];
    [|-40; -20;   0;   0;   0;   0; -20; -40|];
    [|-50; -40; -30; -30; -30; -30; -40; -50|]
  |]

  let bishop_table =
  [|
    [|-20; -10; -40; -10; -10; -40; -10; -20|]; 
    [|-10;   5;   0;   0;   0;   0;   5; -10|];
    [|-10;  10;  10;  10;  10;  10;  10; -10|];
    [|-10;   0;  10;  10;  10;  10;   0; -10|];
    [|-10;   5;   5;  10;  10;   5;   5; -10|];
    [|-10;   0;   5;  10;  10;   5;   0; -10|];
    [|-10;   0;   0;   0;   0;   0;   0; -10|];
    [|-20; -10; -10; -10; -10; -10; -10; -20|]
  |]

  let king_table =
  [|
    [| 20;  30;  10;   0;   0;  10;  30;  20|];
    [| 20;  20;   0;   0;   0;   0;  20;  20|];
    [|-10; -20; -20; -20; -20; -20; -20; -10|];
    [|-20; -30; -30; -40; -40; -30; -30; -20|];
    [|-30; -40; -40; -50; -50; -40; -40; -30|];
    [|-30; -40; -40; -50; -50; -40; -40; -30|];
    [|-30; -40; -40; -50; -50; -40; -40; -30|];
    [|-30; -40; -40; -50; -50; -40; -40; -30|]
  |]

  let get_table pct = 
    match pct with
      | B.Pawn ->  pawn_table
      | B.Knight ->knight_table
      | B.Bishop ->bishop_table
      | B.Rook   ->pawn_table
      | B.Queen  ->pawn_table
      | B.King   ->king_table

  let value_of_pos pos pc = 
    let (rank,file) = B.pos_to_coord pos in
    match pc with
      | B.Black pt -> (get_table pt).(rank).(file)
      | B.White pt -> (get_table pt).(7 - rank).(7 - file)

  let comp a b =
    match (a, b) with
      | (Inf, Inf) | (NInf, NInf) -> Order.Equal
      | (Inf, _) | (_, NInf) -> Order.Greater
      | (NInf, _) | (_, Inf) -> Order.Less
      | (Finite a, Finite b) ->
          let cmp = compare a b in
            if cmp > 0 then Order.Greater
            else if cmp = 0 then Order.Equal
            else Order.Less
  let negate a =
    match a with
      | Inf -> NInf
      | NInf -> Inf
      | Finite a -> Finite (-a)
  let apply eval bd = eval bd
  let init_eval bd =
    let pcs = B.all_pieces bd in
    let pc_dir pc =
      match pc with
        | B.Black _ -> -1
        | B.White _ -> 1 in
    let pc_val pc =
      (match pc with
         | B.Black B.Pawn | B.White B.Pawn -> 100
         | B.Black B.Knight | B.White B.Knight -> 320
         | B.Black B.Bishop | B.White B.Bishop -> 325
         | B.Black B.Rook | B.White B.Rook -> 500
         | B.Black B.Queen | B.White B.Queen -> 975
         | B.Black B.King | B.White B.King -> 32767
      ) * pc_dir pc * pc_dir (B.to_play bd) in
    let ck = if (B.check bd) then -5 else 0 in
    let ckmt = if (B.checkmate bd) then -100000 else 0 in
    let eval_binding r (pos, pc) = r + (pc_val pc) + (value_of_pos pos pc) in
      Finite(ck + ckmt + List.fold_left eval_binding 0 pcs)
  let train eval bd v = eval
end

module NNEval (B : BOARD) (N : NN) : (EVAL with type board = B.board) =
struct
  type board = B.board
  type value = float
  type evaluator = N.t
  let ubound = infinity
  let lbound = neg_infinity
  let comp a b =
    let result = compare a b in
      if result < 0 then Order.Less
      else if result = 0 then Order.Equal
      else Order.Greater
  let negate = ( *. ) (-1.0)
  let init_eval = N.create 384 32 1
  let translate bd =
    let pc_index pc = match pc with
      | B.White B.Pawn | B.Black B.Pawn -> 0
      | B.White B.Knight | B.Black B.Knight -> 64
      | B.White B.Bishop | B.Black B.Bishop -> 128
      | B.White B.Rook | B.Black B.Rook -> 192
      | B.White B.Queen | B.Black B.Queen -> 256
      | B.White B.King | B.Black B.King -> 320
    in
    let pc_val pc = match pc with
      | B.White pc -> 1.0
      | B.Black pc -> -1.0
    in
    let input = Array.make 384 0.0 in
    let pieces = B.all_pieces bd in
    let add_to_array (pos, pc) = 
      let (rank, file) = B.pos_to_coord pos in
	    Array.set input (pc_index pc + rank + file * 8) (pc_val pc)
    in
    let _ = List.map add_to_array pieces in
      input
  let apply (e: evaluator) (bd: board) : value =
    let input = translate bd in (N.eval e input).(0)
  let train eval bd v =
    let input = translate bd in N.train eval input [|v|]
end

(* an engine using minimax search based on an evaluator *)
module MinimaxEngine (B : BOARD)
  (L : EVAL with type board = B.board) (R : ENGPARAMS) :
  (ENGINE with type board = B.board
    and type move = B.move
    and type evaluator = L.evaluator) =
struct
  type board = B.board
  type move = B.move
  type evaluator = L.evaluator

  let rec score eval bd =
    let rec score_r n bd =
      if n <= 0 then L.apply eval bd
      else
        let rec score_moves v mvs =
          match mvs with
            | [] -> L.lbound
            | mv :: tl ->
                match B.play bd mv with
                  | None -> score_moves v tl
                  | Some result ->
                      let v' = L.negate (score_r (n - 1) result) in
                        match L.comp v' v with
                          | Order.Less | Order.Equal -> v
                          | Order.Greater -> v'
        in score_moves L.lbound (B.all_moves bd)
    in score_r R.depth bd
  
  let rec strat eval bd =
    let _ = Random.self_init () in
    let eval_move mv = match B.play bd mv with
      | None -> (mv, L.lbound)
      | Some bd -> (mv, score eval bd) in
    let choose_move (mv1, v1) (mv2, v2) =
      match L.comp v1 v2 with
        | Order.Less -> (mv1, v1)
        | Order.Equal -> if Random.bool() then (mv1, v1) else (mv2, v2)
        | Order.Greater -> (mv2, v2)
    in
      match B.all_moves bd with
        | [] -> None
        | mv :: tl -> 
            let evaluated = List.map eval_move tl in
              List.fold_left choose_move (eval_move mv) evaluated
end

(* an engine using alpha-beta search based on an evaluator *)
module ABEngine (B : BOARD)
  (L : EVAL with type board = B.board) (R : ENGPARAMS) :
  (ENGINE with type board = B.board
    and type move = B.move
    and type evaluator = L.evaluator) =
struct
  type board = B.board
  type move = B.move
  type evaluator = L.evaluator

  let rec score eval bd =
    let rec score_r n a b bd =
      if n <= 0 then L.apply eval bd
      else
        let rec score_moves a b mvs =
          match mvs with
            | [] -> a
            | mv :: tl ->
                match B.play bd mv with
                  | None -> score_moves a b tl
                  | Some result ->
                      let rec_a = L.negate b in
                      let rec_b = L.negate a in
                      let v = L.negate (score_r (n - 1) rec_a rec_b result) in
                        match (L.comp a v, L.comp b v) with
                          | (Order.Less, Order.Less) -> a
                          | (Order.Less, _) ->
                              score_moves v b tl
                          | (Order.Greater, _) | (Order.Equal, _) ->
                              score_moves a b tl
        in score_moves a b (B.all_moves bd)
    in score_r R.depth L.ubound L.lbound bd
  
  let rec strat eval bd =
    let _ = Random.self_init () in
    let eval_move mv =
      match B.play bd mv with
        | None -> (mv, L.lbound)
        | Some bd -> (mv, L.score eval bd) in
    let choose_move (mv1, v1) (mv2, v2) =
      match L.comp v1 v2 with
        | Less -> (mv1, v1)
        | Equal ->  if Random.bool () then (mv1, v1) else (mv2, v2)
        | Greater -> (mv2, v2)
    in
      match B.all_moves bd with
        | [] -> None
        | mv :: tl -> 
            let evaluated = List.map eval_move tl in
              List.fold_left choose_move (eval_move mv) evaluated
end

(* Standard synonyms so we can easily change implementation *)
module StdParams : ENGPARAMS =
struct
  let depth = 3
end
module StdEval : (EVAL with type board = StdBoard.board) =
  SimpleEval (StdBoard)
module StdEngine : (ENGINE with type board = StdBoard.board
                    and type move = StdBoard.move
                    and type evaluator = StdEval.evaluator) =
  ABEngine (StdBoard) (StdEval) (StdParams)
