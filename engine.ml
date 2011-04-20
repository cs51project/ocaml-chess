open Board

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
  val apply : evaluator -> board -> value
  val init_eval : evaluator (* standard evaluator *)
  val train : evaluator -> evaluator (* learning function *)
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
      match (pc, B.to_play bd) with
        | (B.Black _, B.White _) | (B.White _, B.Black _) -> -1
        | (B.White _, B.White _) | (B.Black _, B.Black _) -> 1 in
    let pc_val pc =
      (match pc with
         | B.Black B.Pawn | B.White B.Pawn -> 1
         | B.Black B.Knight | B.White B.Knight -> 3
         | B.Black B.Bishop | B.White B.Bishop -> 3
         | B.Black B.Rook | B.White B.Rook -> 5
         | B.Black B.Queen | B.White B.Queen -> 9
         | B.Black B.King | B.White B.King -> 1000
      ) * pc_dir pc * pc_dir (B.to_play bd)
    in
    let eval_binding r (_, pc) = r + pc_val pc in
      Finite (List.fold_left eval_binding 0 pcs)
  let train eval = eval
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

  let score eval strat1 strat2 bd =
    let rec signed_score bd strat1 strat2 color n =
      let sign =
        match (color, B.to_play bd) with
          | (B.Black _, B.Black _) | (B.White _, B.White _) -> (fun x -> x)
          | (B.Black _, B.White _) | (B.White _, B.Black _) -> L.negate
      in
        if n = 0 then sign (L.apply eval bd)
        else
          match strat1 bd with
            | None -> sign (L.apply eval bd)
            | Some mv ->
                match B.play bd mv with
                  | None -> sign (L.apply eval bd)
                  | Some bd2 -> signed_score bd2 strat2 strat1 color (n - 1)
    in
      signed_score bd strat1 strat2 (B.to_play bd) R.depth
  
  let rec best_against strat eval bd =
    let score = score eval strat (best_against strat eval) in
    let choose_move mv1 mv2 =
      match mv1 with
        | None -> Some mv2
        | Some mv1 ->
            match (B.play bd mv1, B.play bd mv2) with
              | (None, None) -> None
              | (Some _, None) -> Some mv1
              | (None, Some _) -> Some mv2
              | (Some bd1, Some bd2) ->
                  match L.comp (score bd1) (score bd2) with
                    | Order.Less | Order.Equal -> Some mv1
                    | Order.Greater -> Some mv2
    in
      match B.all_moves bd with
        | [] -> None
        | hd :: tl -> List.fold_left choose_move (Some hd) tl
  
  let rec strat eval = best_against (strat eval) eval
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
                          | (Order.Less, _) -> score_moves v b tl
                          | (Order.Greater, _) | (Order.Equal, _) ->
                              score_moves a b tl
        in score_moves a b (B.all_moves bd)
    in score_r R.depth L.ubound L.lbound bd
  
  let rec strat eval bd =
    let choose_move mv1 mv2 =
      match mv1 with
        | None -> Some mv2
        | Some mv1 ->
            match (B.play bd mv1, B.play bd mv2) with
              | (None, None) -> None
              | (Some _, None) -> Some mv1
              | (None, Some _) -> Some mv2
              | (Some bd1, Some bd2) ->
                  match L.comp (score eval bd1) (score eval bd2) with
                    | Order.Less | Order.Equal -> Some mv1
                    | Order.Greater -> Some mv2
    in
      match B.all_moves bd with
        | [] -> None
        | hd :: tl -> List.fold_left choose_move (Some hd) tl
end

(* Standard synonyms so we can easily change implementation *)
module StdParams : ENGPARAMS =
struct
  let depth = 4
end
module StdEval : (EVAL with type board = StdBoard.board) =
  SimpleEval (StdBoard)
module StdEngine : (ENGINE with type board = StdBoard.board
                    and type move = StdBoard.move
                    and type evaluator = StdEval.evaluator) =
  ABEngine (StdBoard) (StdEval) (StdParams)
