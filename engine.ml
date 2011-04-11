open Util
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
  val strat : board -> evaluator -> move option
end

module StdEval (B : BOARD) =
struct
  type board = B.board
  type value = int
  type evaluator = board -> value
  let comp a b =
    let cmp = compare a b in
    if cmp > 0 then Greater
    else if cmp = 0 then Equal
    else Less
  let negate = ( * ) -1
  let apply eval bd = eval bd
  let init_eval bd =
    let pcs = B.all_pieces bd in
    let pc_dir pc =
      match pc with
        | Black _ -> -1
        | White _ -> 1 in
    let pc_val pc =
      (match pc with
         | Black Pawn | White Pawn -> 1
         | Black Knight | White Knight -> 3
         | Black Bishop | White Bishop -> 3
         | Black Rook | White Rook -> 5
         | Black Queen | White Queen -> 9
         | Black King | White King -> 1000
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
          | (Black _, Black _) | (White _, White _) -> (fun x -> x)
          | (Black _, White _) | (White _, Black _) -> L.negate
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
      match (B.play bd mv1, B.play bd mv2) with
        | (None, None) -> None
        | (Some _, None) -> mv1
        | (None, Some _) -> mv2
        | (Some bd1, Some bd2) ->
            match L.compare (score bd1) (score bd2) with
              | Order.Less | Order.Equal -> mv1
              | Order.Greater -> mv2
    in
      match B.all_moves bd with
        | [] -> None
        | hd :: tl -> List.fold_left choose_move hd tl
  
  let strat eval = best_against strat eval
end