module type NN =
sig
  type input
  type output
  type t
  val create : int -> int -> int -> t
  val eval : t -> input -> output
  val train : t -> input list -> output list -> t
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
    else Array.mapi (fun i v_i -> v_i *. w.(i))
  
  let outer v w = Array.map (scalar_mul w) v
  
  let inner v w = Array.fold_left (+.) 0.0 (mul_comps v w)
  
  let quad_norm v =
    Array.fold_left (fun r x -> (x *. x /. 2) +. r) 0.0 v

  let sigmoid x = tanh (x /. 2)

  let vector_sigmoid v = Array.map sigmoid v
  
  (* m : MxN; v : Nx1 *)
  let matrix_mul_r m v = Array.map (inner v) m
  
  let matrix_mul_l v m =
    let rows = Array.mapi (function i v_i -> scalar_mul m.(i) v_i) in
      Array.fold_left add (zero (Array.length v)) rows
  (***************************************************)
  
  let create in_dim hidden_dim out_dim =
    Random.self_init ();
    let box1 = Array.make_matrix hidden_dim in_dim Random.float in
    let box2 = Array.make_matrix out_dim hidden_dim Random.float in
    let o1 = Array.map (Array.map (fun f -> f 1)) box1 in
    let o2 = Array.map (Array.map (fun f -> f 1)) box2 in
      {o1; o2}
  
  let feed_forward nn data =
    let o1 = vector_sigmoid (matrix_mul nn.w1 data) in 
    let o2 = vector_sigmoid (matrix_mul nn.w2 o1) in (o1, o2)

  let eval nn data =
    snd (feed_forward nn data)

  let backprop nn sample expected =
    let (o1, o2) = feed_forward nn sample in
    let err = sub expected o2 in
    let d2 = Array.map (fun x -> (1 -. x *. x) /.2) o2 in
    let delta2 = (mul_comps d2 err) in
    let d1 = Array.map (fun x -> (1 -. x *. x) /.2) o1 in
    let delta1 = (mul_comps d2 (matrix_mul_left delta2 nn.w2)) in
    let w2' = Array.mapi (fun i w2_i -> Array.mapi (fun j w2_ij -> w2_ij -.
      (o1.(j) *. delta2.(i))) w2_i) nn.w2 in
    let w1' = Array.mapi (fun i w1_i -> Array.mapi (fun j w1_ij -> w1_ij -.
      (sample.(j) *. delta1.(i))) w1_i) nn.w1
    in {w1; w2}

  let train nn inputs outputs =
    List.fold_left2 backprop nn inputs outputs
end
