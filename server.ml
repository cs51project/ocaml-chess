open Util

(* Web server for user interface:
 * GUI is displayed via HTML and CSS on a browser, and AJAX
 * is used to transmit user moves to the chess engine.
 * 
 * Adapted from moogle web server.
 *)

let debug = false

(* Read the command line arguments and return the 
 * port number which OCamlChess should use for serving. *)
let server_port = 
  let args = Sys.argv in 
    try 
      int_of_string (Array.get args 1)
    with _ -> 
      (Printf.printf 
         "usage: %s <port>\n" 
         (Array.get args 0) ;
       exit 1)

let response_header mime = 
  "HTTP/1.1 200 OK\n" ^
  "Server: OCamlChess/1.0\n" ^
  "Content-Type: " ^ mime ^ "\n" ^
  "Content-Language: en-us\n" ^
  "Connection: close\n\n"

let fail_header =
  "HTTP/1.1 404 NOT FOUND\n" ^
  "Server: OCamlChess/1.0\n" ^
  "Connection: close\n\n"

(* A post request will have a bunch of headers
 * on it separated from the actual data by two newlines (or two
 * carriage-returns/line-feeds.)  This finds those two spaces and
 * strips off all the headers. *)
let strip_headers post = 
  let rec find_two_newlines i = 
    if i+2 < String.length post then
      match String.sub post i 2 with 
        | "\n\n" -> Some (i+2)
        | "\r\n" -> 
            if i+4 < String.length post then 
              (match String.sub post (i+2) 2 with 
                 | "\r\n" -> Some (i+4)
                 | _ -> find_two_newlines (i+1))
            else None
        | _ -> find_two_newlines (i+1)
    else None
  in 
    match find_two_newlines 0 with 
      | None -> post
      | Some i -> String.sub post i (String.length post - i)

(* A post request is encoded in URL form.  This function extracts
 * the key-value pairs.
 *)
let url_decode request =
  let bindings = Str.split (Str.regexp_string "&") request in
  let binding_to_pair str =
    let binding_re = Str.regexp_case_fold "^\\([^ \t]+\\)=\\([^ \t]+\\)$" in
      if Str.string_match binding_re str 0 then
        let k = Str.matched_group 1 str in
        let v = Str.matched_group 2 str in
        let decode_re = Str.regexp_string "+" in
        let decoded_k = Str.global_replace decode_re " " k in
        let decoded_v = Str.global_replace decode_re " " v in
          Some(decoded_k, decoded_v)
      else None
  in
    deoptionalize (List.map binding_to_pair bindings)

(* Given a requested path, return the corresponding local path *)
let local_path qs =
  Filename.concat (Unix.getcwd()) qs

(* read in all the lines from a file and concatenate them into
 * a big string. *)
let rec input_lines inchan lines = 
  try 
    input_lines inchan ((input_line inchan)::lines)
  with End_of_file -> List.rev lines

let read_text file =
  let _ = flush_all () in
  let ch = open_in file in
  let lines = input_lines ch [] in
  let resp = String.concat "\n" lines in
    close_in ch ; resp

let read_bin file =
  let _ = flush_all () in
  let ch = open_in_bin file in
  let rec read_bin_r str =
    try
      read_bin_r (str ^ String.make 1 (input_char ch))
    with End_of_file -> str
  in
    read_bin_r ""

let read_file file =
  let ext =
    let ext_re = Str.regexp_case_fold "\\.\\([a-z]+\\)$" in
      try
        let _ = Str.search_forward ext_re file 0 in
          Str.matched_group 1 file
      with Not_found -> ""
  in
  let text_response mime =
    (response_header (mime ^ "; charset=utf-8")) ^
    (read_text file)
  in
  let bin_response mime =
    (response_header mime) ^ (read_bin file)
  in
    if ext = "html" then text_response "text/html"
    else if ext = "css" then text_response "text/css"
    else if ext = "js" then text_response "text/javascript"
    else if ext = "txt" then text_response "text/plain"
    else if ext = "png" then bin_response "image/png"
    else if ext = "ico" then bin_response "image/vnd.microsoft.icon"
    else bin_response "application/octet_stream"

let std_response =
  read_file "./index.html"

let send_std_response client_fd =
  Unix.send client_fd std_response 0 (String.length std_response) []

let send_all fd buf =
  let rec more st size = 
    let res = Unix.send fd buf st size [] in
    if res < size then
      more (st + res) (size - res)
    else ()
  in 
  let size = String.length buf in 
  let _ = more 0 size in size

let http_get_re = 
  Str.regexp_case_fold "GET[ \t]+/\\([^ \t]*\\)[ \t]+HTTP/1\\.[0-9]"

let http_post_re =
  Str.regexp_case_fold "POST[ \t]+/index\\.html[ \t]+HTTP/1\\.[0-9]"

let process_request client_fd request =
  let is_safe s = 
    (* At least check that the passed in path doesn't contain .. *)
    let r = Str.regexp_string ".." in
      try
        let _ = Str.search_forward r s 0 in
          false
      with Not_found -> true
  in
    if Str.string_match http_get_re request 0 then
      let query_string = Str.matched_group 1 request in
      let response =
        if is_safe query_string then
          let path = local_path query_string in
            try
              match (Unix.stat path).Unix.st_kind with
                | Unix.S_REG -> read_file path
                | Unix.S_DIR ->
                    read_file (Filename.concat path "index.html")
                | _ -> fail_header
            with _ -> std_response
        else fail_header
      in
        send_all client_fd response
    else if Str.string_match http_post_re request 0 then
      let data_urlencoded = strip_headers request in
        send_all client_fd data_urlencoded
    else send_all client_fd fail_header
;;

(* The server loop, adapted from moogle. *)
let server () = 
  let fd = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let sock_addr = Unix.ADDR_INET (Unix.inet_addr_any, server_port) in
  let _ = Unix.setsockopt fd Unix.SO_REUSEADDR true in
  let _ = Unix.bind fd sock_addr in
  let _ = Unix.listen fd 5 in  (* at most 5 queued requests *)
  (* loop on a set of client-server games *)
  let rec server_loop () = 
    (* allow a client to connect *)
    let (client_fd, client_addr) = Unix.accept fd in 
    let buf = String.create 4096 in
    let len = Unix.recv client_fd buf 0 (String.length buf) [] in
    let request = String.sub buf 0 len in
    let _ = process_request client_fd request in
      Unix.close client_fd ; 
      server_loop ()
  in 
    server_loop ()

let main () =
  let _ =
    if debug then Printf.printf
      "Starting chess engine on port %d..." server_port
    else ()
  in
    server ()
;;

main ()
