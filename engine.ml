open Board


module Order =
struct
  type order = Less | Equal | Greater
end

module type EVAL =
sig
  type board

  (* Calculate the board's value to the current player. *)
  type value
  val eval : board -> value
  val compare : value -> value -> Order.order
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

(* an engine using alpha-beta search based on an evaluator *)
module ABSEngine (B : BOARD) (L : EVAL with type board = B.board and type value = int)
  (R : ENGPARAMS) :
  (ENGINE with type board = B.board and type move = B.move) =
struct
  type board = B.board
  type move = B.move

  let move_eval bd mv =
    let rec move_eval_r n bd =
      if n = 0 then L.eval bd
      else
        let moves = B.all_moves bd in
          match moves with
            | [] -> L.eval
            | _ -> 
                let boards = List.map (B.play bd) moves in
                let values = List.map (move_eval_r (n - 1)) boards in
                  List.fold_left max -1000000 values
    in
      move_eval_r R.depth (B.play bd mv)

  let strat bd =
    let moves = B.all_moves bd in
    let better_move mv1 mv2 =
      if move_eval bd mv1 > move_eval bd mv2 then mv1
      else mv2
    in
      match moves with
        | [] -> None
        | hd::tl -> 
            Some (List.fold_left better_move hd moves)
end

