type day = int
type version = int

type mode = Practice of day * version | Final of day

let get_lines mode = 
  match mode with 
  | Practice (day, version) -> 
    let path : string = Printf.sprintf "input/day%d/prac%d.txt" day version in 
    In_channel.with_open_bin path (function file -> In_channel.input_lines file)
  | Final day -> 
    let path : string = Printf.sprintf "input/day%d/final.txt" day in 
    In_channel.with_open_bin path (function file -> In_channel.input_lines file)
