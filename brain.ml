module type NET =
sig
  type input
  type output
  type t
  exception Bad_op (* Raised if dimensions do not match *)
  val create : t
  val eval : t -> input -> output
  val train : t -> input -> output -> t 
end

(* A Neural Network based on Error Backpropagation. *)
module BPNet : NET =
struct
  type input = float array
  type output = float array
  type matrix = float array array
  type t = {syn1: matrix; syn2: matrix; syn3: matrix}
  exception Bad_op

  (* m : MxN; v : Nx1 *)
  let mult m v =
    if Array.length m = 0 or Array.length m.(0) != Array.length v
    then raise Bad_op
    else
      let do_row r =
        Array.fold_left (+.) 0 (Array.mapi (fun j m_ij -> m_ij *. v.(j)) r)
      in Array.map do_row m

  let sub v w = Array.mapi (fun i v_i -> v_i -. w.(i)) v
  
  let norm v = sqrt (Array.fold_left (fun r x -> x *. x +. r) 0 v)
  
  let rms v = norm v /. sqrt (Array.length v)

  let eval net data =
    let l1 = mult net.syn1 data in
    let l2 = mult net.syn2 l1 in
      mult net.syn3 l2

  let error net data exp =
    let actual = eval net data in
    let diff = sub exp actual in
      rms diff

  let train net data exp =
    let e = error net data exp in
    
end
