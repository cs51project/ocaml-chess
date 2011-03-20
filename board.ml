module type BOARD = 
sig
  type key (* position *)   
  type value (* piece *)
  type color 
  type dict 
  val empty : dict 
  val start : dict
  val move : dict -> key -> value -> color -> dict option
    (* should return none if the move is invalid *)
    (* if the move is valid, take the dict and check if the move captures another piece *)
  val capture: dict -> key -> value -> color -> dict
    (* must check if piece is opposite color; else returns original board *)
  val lookup : dict -> key -> color -> (value * color) option
  val remove : dict -> key -> dict
  val check : dict -> color option (* returns color that is in position to win *)
  val checkmate: dict -> color option (* checks if the game is over *)
  val fold : (key -> value -> 'a -> 'a) -> 'a -> dict -> 'a
end
