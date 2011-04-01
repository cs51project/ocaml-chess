#!ocaml

# Learning Chess Engine #
## CS51 Project Functional Specification ##

Identification:

* Han He : hanhe333@gmail.com
* Robert Hero: robertmhero@gmail.com
* David Palmer: dpalmer93@gmail.com
* Amy Yin: amymichelleyin@gmail.com

### Brief Overview ###

We aim to implement the game of chess and develop two artificial intelligence chess engines.  We will first implement one engine based on a search tree and a standard evaluator function.  Then we plan to compare this engine to our “experimental” engines that will “learn” how to play chess.

The challenge of developing chess engines is that as the depth of brute-force search increases, the number of possible moves to evaluate increases exponentially, making it impossible to analyze all outcomes. An effective chess engine must handle this mass of data efficiently so that it runs within the time frame of an average chess move and nonetheless plays well against a human opponent. One solution to this problem is to use heuristics to cut down the number of positions that must be evaluated.  However, inventing these heuristics requires extensive knowledge of chess strategy. Instead, we aim to teach our engine its heuristics using two machine learning systems: neural networks and genetic algorithms. Our goal is to create a functional system that behaves at least at the level of the average human being. Ideally, we would like our program to be able to defeat human players.

### Interface Design ###

We have divided the program into various orthogonal modules.  Thus one interface defines the board and pieces, another defines the chess engine, and another defines the learning evaluator used in the engine.

#### Board ####

The board is built using a map from positions on the board to the pieces occupying those positions.  Positions are abstract, but internally represented by pairs of coordinates. Each piece is defined by a color and name. Standard moves are defined by starting and ending positions; castling moves are defined separately.  Other special moves, such as *En Passant* and Promotion, will be defined as standard moves, but will have special effects when played. We have functions to check for check and checkmate. These checks will be performed whenever a move is made.

Here is the signature:

    module type BOARD =
    sig
      type position
      type piece_type = Pawn | Knight | Bishop | Rook | Queen | King
      type piece = Black of piece_type | White of piece_type
      (* encode Black as Black King, White as White King *)
      type side = piece
      type castle = Queenside | Kingside
      type move = Standard of position * position | Castle of castle
      type board
      exception InvalidPosition
    
      (* build position from pair of integers
      * (i.e. create_pos rank file,
      * where rank, file are between 0 and 7
      * raise exception if invalid coordinates *)
      val create_pos : int -> int -> position
      (* functions for manipulating positions *)
      val neighbor : int -> int -> position -> position option
      val vector : position -> position -> (int * int)
      (* standard starting board *)
      val init_board : board
      (* which color is to play *)
      val to_play : board -> side
      (* all pieces on current board *)
      val all_pieces : board -> (position, piece) list
      (* is move valid? *)
      val is_valid : board -> move -> bool
      (* all valid moves *)
      val all_moves : board -> move list
      (* what piece is at given position *)
      val lookup : position -> board -> piece option
      (* should return None if the move is invalid *)
      val play : board -> move -> board option
      (* returns color in check or None *)
      val check : board -> side option
      (* returns losing color or None *)
      val checkmate: board -> side option
      val can_castle: board -> bool
    end

#### Chess Engine ####

The chess engine is based on a minimax algorithm. The engine searches through possible positions at a specified depth, looking for those positions which are most favorable.  In order to compare the values of positions, the engine uses an evaluator, which is an abstract data structure that can be applied to a board to produce a value. The standard evaluation function only adds up the values of the pieces of each color and subtracts the opposing sums. However, future evaluators will be able to learn better heuristics for evaluating boards by way of the `train` function. We will also improve the core engine by implementing Alpha-Beta pruning, which dramatically reduces the number of positions that the engine must consider in order to find the best move.

Here are the signatures:
An Order module, used for comparing evaluator values:
    module Order =
    struct
      type order = Less | Equal | Greater
    end

The evaluator interface:
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

Parameter for the engine, specifying search depth:
    module type ENGPARAMS =
    sig
      val depth : int
    end

And the engine itself:
    module type ENGINE =
    sig
      type board
      type move
      val strat : board -> move option
    end

### Algorithms ###

We have implemented a basic minimax algorithm without alpha-beta pruning in our engine.ml file. This algorithm calculates all possible moves from a given board, then evaluates each one recursively by looking at the relative values of possible future board configurations that may arise if it is played.  Here is our implementation of minimax in two functors parametrized by a board implementation and an evaluator implementation, as well as a module of type `ENGPARAMS` that specifies the depth of search.

    module ABSEngine (B : BOARD) (L : EVAL with type board = B.board)
      (R : ENGPARAMS) :
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
                      List.fold_left L.max min_value values
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