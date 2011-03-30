open Board


module Order =
struct
  type order = Less | Equal | Greater
end


module type EVAL =
sig
  type board
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
  val strat : board -> move
end

(* an engine using alpha-beta search based on an evaluator *)
module ABSEngine (B : BOARD) (L : EVAL with type board = B.board)
  (R : ENGPARAMS) :
  (ENGINE with type board = B.board and type move = B.move) =
struct
  type board = B.board
  type move = B.move
  
  let strat bd =
    let rec strat_r bd maxval level =
      let moves = B.all_moves bd in
      let boards = List.map (play B.bd) moves in
      let 
    in
      
end

