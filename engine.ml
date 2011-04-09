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
  val min_value : value (* minimum possible value *)
  val comp : value -> value -> Order.order
  val max : value -> value -> value
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
  val strat : board -> move option
end

module StdEval (B : BOARD) =
struct
  type board = B.board
  type value = int
  type evaluator = board -> value
  let min_value = -1000000
  let comp a b =
    let cmp = compare a b in
    if cmp > 0 then Greater
    else if cmp = 0 then Equal
    else Less
  let max = max
  let apply eval bd = eval bd
  let init_eval bd =
    let pcs = B.all_pieces bd in
    let pc_side pc =
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
      ) * pc_side pc * pc_side (B.to_play bd)
    in
      List.fold_left (fun r elt -> let (_, pc) = elt in r + pc_val pc) 0 pcs
  let train eval = eval
end

(* an engine using minimax search based on an evaluator *)
module MinimaxEngine (B : BOARD)
  (L : EVAL with type board = B.board) (R : ENGPARAMS) :
  (ENGINE with type board = B.board and type move = B.move) =
struct
  type board = B.board
  type move = B.move

  let move_eval bd mv eval =
    let rec move_eval_r n bd =
      if n = 0 then L.apply eval bd
      else
        let moves = B.all_moves bd in
          match moves with
            | [] -> L.apply eval bd
            | _ -> 
                let boards = List.map (B.play bd) moves in
                let values = List.map (move_eval_r (n - 1)) boards in
                  List.fold_left L.max L.min_value values
    in
      move_eval_r R.depth (B.play bd mv)

  let strat bd eval =
    let moves = B.all_moves bd in
    let better_move mv1 mv2 =
      match comp (move_eval bd mv1 eval) (move_eval bd mv2 eval) with
        | Greater -> mv1
        | Equal | Less -> mv2
    in
      match moves with
        | [] -> None
        | hd::tl ->
            Some (List.fold_left better_move hd moves)
end

