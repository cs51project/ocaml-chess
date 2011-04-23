module FancyEval (B: BOARD) : (Eval with type board = B.board) =
struct 
  type board = B.board
  type value = Finite of int | Inf | NInf
  type evaluator = board ->  