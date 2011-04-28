module type NN =
sig
  type input = float array
  type output = float array
  type t
  val create : int -> int -> int -> t
  val eval : t -> input -> output
  val train : t -> (input * output) list -> t
end

(* feedforward neural network trained via backpropagation *)
module BPNet : NN =
struct
  type input = float array
  type output = float array
  type matrix = float array array
  type t = {w1: matrix; w2: matrix}
  exception Bad_op

  (**************** Linear Algebra ****************)
  let zero d = Array.make d 0.0
  
  let add v w =
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i +. w.(i)) v
  
  let sub v w =
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i -. w.(i)) v
  
  let scalar_mul v a = Array.map (( *. ) a) v
  
  let mul_comps v w = 
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i *. w.(i)) v
  
  let outer v w = Array.map (scalar_mul w) v
  
  let inner v w = Array.fold_left (+.) 0.0 (mul_comps v w)
  
  let quad_norm v =
    (Array.fold_left (fun r x -> x *. x +. r) 0.0 v) /. 2.0

  let sigmoid x = tanh (x /. 2.0)

  let vector_sigmoid v = Array.map sigmoid v
  
  (* m : MxN; v : Nx1 *)
  let matrix_mul_r m v = Array.map (inner v) m
  
  (* v : 1xM; m : MxN *)
  let matrix_mul_l v m =
    let rows = Array.mapi (fun i m_i -> scalar_mul m_i v.(i)) m in
      Array.fold_left add (zero (Array.length m.(0))) rows
  (***************************************************)
  
  let create in_dim hidden_dim out_dim =
    Random.self_init ();
    let box1 = Array.make_matrix in_dim hidden_dim Random.float in
    let box2 = Array.make_matrix hidden_dim out_dim Random.float in
    let w1 = Array.map (Array.map (fun f -> (f 2.0) -. 1.0)) box1 in
    let w2 = Array.map (Array.map (fun f -> (f 2.0) -. 1.0)) box2 in
      {w1; w2}
  
  let feed_forward nn data =
    let o1 = vector_sigmoid (matrix_mul_l data nn.w1) in 
    let o2 = vector_sigmoid (matrix_mul_l o1 nn.w2) in (o1, o2)

  let eval nn data =
    snd (feed_forward nn data)

  (* SEE R. Rojas: Neural Networks, Springer-Verlag, Berlin, 1996 *)
  let backprop rate nn (sample, expected) =
    let (o1, o2) = feed_forward nn sample in
    let err = sub o2 expected in
    let d2 = Array.map (fun x -> (1.0 -. x *. x) /. 2.0) o2 in
    let delta2 = (mul_comps d2 err) in
    let d1 = Array.map (fun x -> (1.0 -. x *. x) /. 2.0) o1 in
    let delta1 = (mul_comps d1 (matrix_mul_r nn.w2 delta2)) in
    let w2' = Array.mapi (fun i w2_i -> Array.mapi (fun j w2_ij -> w2_ij -.
      (o1.(i) *. delta2.(j) *. rate)) w2_i) nn.w2 in
    let w1' = Array.mapi (fun i w1_i -> Array.mapi (fun j w1_ij -> w1_ij -.
      (sample.(i) *. delta1.(j) *. rate)) w1_i) nn.w1
    in {w1 = w1'; w2 = w2'}

  let train nn patterns =
    List.fold_left (backprop 0.1) nn patterns
end

(* feedforward neural network trained via backpropagation *)
module ParallelNet : NN =
struct
  type input = float array
  type output = float array
  type matrix = float array array
  type t = {w1: matrix; w2: matrix}
  exception Bad_op

  (**************** Linear Algebra ****************)
  let zero d = Array.make d 0.0
  
  let add v w =
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i +. w.(i)) v
  
  let sub v w =
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i -. w.(i)) v
  
  let scalar_mul v a = Array.map (( *. ) a) v
  
  let mul_comps v w = 
    if Array.length v != Array.length w then raise Bad_op
    else Array.mapi (fun i v_i -> v_i *. w.(i)) v
  
  let outer v w = Array.map (scalar_mul w) v
  
  let inner v w = Array.fold_left (+.) 0.0 (mul_comps v w)
  
  let quad_norm v =
    (Array.fold_left (fun r x -> x *. x +. r) 0.0 v) /. 2.0

  let sigmoid x = tanh (x /. 2.0)

  let vector_sigmoid v = Array.map sigmoid v
  
  (* m : MxN; v : Nx1 *)
  let matrix_mul_r m v = Array.map (inner v) m
  
  (* v : 1xM; m : MxN *)
  let matrix_mul_l v m =
    let rows = Array.mapi (fun i m_i -> scalar_mul m_i v.(i)) m in
      Array.fold_left add (zero (Array.length m.(0))) rows
  (***************************************************)
  
  let create in_dim hidden_dim out_dim =
    Random.self_init ();
    let box1 = Array.make_matrix in_dim hidden_dim Random.float in
    let box2 = Array.make_matrix hidden_dim out_dim Random.float in
    let w1 = Array.map (Array.map (fun f -> (f 2.0) -. 1.0)) box1 in
    let w2 = Array.map (Array.map (fun f -> (f 2.0) -. 1.0)) box2 in
      {w1; w2}
  
  let feed_forward nn data =
    let o1 = vector_sigmoid (matrix_mul_l data nn.w1) in 
    let o2 = vector_sigmoid (matrix_mul_l o1 nn.w2) in (o1, o2)

  let eval nn data =
    snd (feed_forward nn data)

  external train : t -> (input * output) list -> t = "nn_train"
end