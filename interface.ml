(* Web server for user interface:
 * GUI is displayed via HTML and CSS on a browser, and AJAX
 * is used to transmit user moves to the chess engine.
 *)

let debug = false

let response_header mime = 
  "HTTP/1.1 200 OK\n" ^
    "Server: OCamlChess/1.0\n" ^
    "content-type: " ^ mime ^ "; charset=utf-8\n" ^
    "Content-Language: en-us\n" ^
    "Connection: close\n\n"

(* The server loop, adapted from moogle. *)
let server init_board = 
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in 
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in 
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  (* loop on a set of client-server games *)
  let rec server_loop boards = 
    (* allow a client to connect *)
    let (client_fd, client_addr) = Unix.accept fd in 
    let buf = String.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    let new_bd = process_request client_fd request init_board boards in
      Unix.close client_fd ; 
      server_loop new_bd
  in 
    server_loop empty

let main () =
  let _ =
    if debug then Printf.printf "Starting chess engine" ^
      "on port %d..." server_port
    else ()
  in
    server init_board

main ()
