open! Base
open Stdlib

let _ = print_endline "printing the first solution"

let rec lines () = 
  match In_channel.input_line In_channel.stdin with 
  | Some value -> value :: (lines ())
  | None -> []

let all_lines = lines ()

(* Could refactor this function *)
let get_first_digit line = 
  let left_digit left right = 
    match 
    (let code = Char.code left in 
    if code = 0 then Some 0
    else 
      (let value = code - (Char.code '1') + 1 in 
      if value >= 1 && value <= 9 
      then Some value 
      else None)) with 
    | Some _ as x -> x 
    | None -> right in 
  match String.fold_right left_digit line None with 
  | Some c -> c
  | None -> failwith "needs to have a digit"

let reverse line = 
  let reversed_char i = 
    String.get line ((String.length line) - i - 1) in 
  String.init (String.length line) reversed_char

let get_last_digit line = 
  get_first_digit (reverse line)

let number line = 
  (get_first_digit line) * 10 + (get_last_digit line)
(* let values = List.map number all_lines *)

(* let () = Stdio.printf "The answer to the first problem is %d\n" (List.fold_left ( + ) 0 values)  *)

let _ = print_endline "printing out the second solution"

let chr i = 
  Char.chr (i + Char.code '1' - 1)

let rec split_by_char i line = 
  if i = 9 then String.split_on_char '9' line
  else (
    let result = split_by_char (i + 1) line in 
    let split_by_i = List.map (String.split_on_char (chr i)) result in 
    List.flatten split_by_i
  )

let num_string line i = 
  let suffix = String.sub line i ((String.length line) - i) in 
  let rec has_prefix lst = 
    match lst with 
    | [] -> None 
    | x :: xs -> 
      if String.starts_with ~prefix:x suffix 
      then Some x
      else has_prefix xs in 
  has_prefix ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let id x = x

let string_to_digit x = 
  match x with 
  | "zero" -> 0
  | "one" -> 1 
  | "two" -> 2 
  | "three" -> 3 
  | "four" -> 4 
  | "five" -> 5
  | "six" -> 6
  | "seven" -> 7
  | "eight" -> 8
  | "nine" -> 9
  | _ -> failwith "not possible"

let new_get_first_digit line = 
  match String.get line 0 with 
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as x -> Char.code x - Char.code '1' + 1
  | _ -> 
  let splits = split_by_char 0 line in 
  match splits with 
  | [] -> failwith "Not possible"
  | x :: _ -> (
    let get_left il acc = 
      match num_string x il with 
      | None -> acc
      | Some _ as x -> x in 
    
    let string_digit = List.fold_right get_left (List.init (String.length x) id) None in 
    match string_digit with 
    | None -> get_first_digit line 
    | Some s -> string_to_digit s
  )
  
let new_get_last_digit line = 
  match String.get line (String.length line - 1) with 
  | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as x -> Char.code x - Char.code '1' + 1
  | _ -> 
  let splits = split_by_char 0 line in 
  match List.rev splits with 
  | [] -> failwith "Not possible"
  | x :: _ -> (
    let get_left acc il = 
      match num_string x il with 
      | None -> acc
      | Some _ as x -> x in 
    let string_digit = List.fold_left get_left None (List.init (String.length x) id) in 
    match string_digit with 
    | None -> get_last_digit line 
    | Some s -> string_to_digit s
  )

let new_number line = 
  (new_get_first_digit line) * 10 + (new_get_last_digit line)

let new_values = List.map new_number all_lines
let () = print_endline ((String.concat ", " (List.map Int.to_string new_values)) ^ "\n")

let () = Stdio.printf "The answer to second problem is %d\n" (List.fold_left ( + ) 0 new_values) 