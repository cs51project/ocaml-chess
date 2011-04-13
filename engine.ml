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
  type value = int
  type evaluator = board -> value
  let comp a b =
    let cmp = compare a b in
    if cmp > 0 then Order.Greater
    else if cmp = 0 then Order.Equal
    else Order.Less
  let negate = ( * ) (-1)
  let apply eval bd = eval bd
  let init_eval bd =
    let pcs = B.all_pieces bd in
    let pc_dir pc =
      match pc with
        | B.Black _ -> -1
        | B.White _ -> 1 in
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
      List.fold_left (fun r elt -> let (_, pc) = elt in r + pc_val pc) 0 pcs
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

(* Standard synonyms so we can easily change implementation *)
module StdParams : ENGPARAMS =
struct
  let depth = 5
end
module StdEval : (EVAL with type board = StdBoard.board) =
  SimpleEval (StdBoard)
module StdEngine : (ENGINE with type board = StdBoard.board
                    and type move = StdBoard.move
                    and type evaluator = StdEval.evaluator) =
  MinimaxEngine (StdBoard) (StdEval) (StdParams)