open Board

module type ENGINE =
sig
  type board
  type move
  val strat : board -> move
end
