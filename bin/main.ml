open! Base
open Stdlib

let rec values () = 
  match In_channel.input_line In_channel.stdin with 
  | Some value -> value :: (values ())
  | None -> []

let () = print_endline (String.concat ", " (values()))
