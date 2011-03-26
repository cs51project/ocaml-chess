open Board

module type ENGINE =
sig
  type engine
  val std_engine
  val model : BOARD.board -> engine
  val play : engine -> BOARD.move
end
