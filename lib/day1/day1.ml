open! Base

let get_first_digit line = 
  let left_digit acc left = 
    match acc with 
    | Some _ as x -> x
    | None -> 
    let code = Char.to_int left in 
    if code = 0 then Some 0
    else let value = code - (Char.to_int '1') + 1 in 
    if value >= 1 && value <= 9 
    then Some value 
    else None in
  match String.fold ~init:None ~f:left_digit line with 
  | Some c -> c
  | None -> failwith "needs to have a digit" 

let get_last_digit line = 
   get_first_digit (String.rev line) 

let problem1 lines = 
  (* Could refactor this function *)
  let number line = 
    (get_first_digit line) * 10 + (get_last_digit line) in 

  let values = List.map ~f:number lines in 

  Int.to_string (List.fold_left ~init:0 ~f:( + ) values)

let problem2 lines = 
  let chr i = 
    match Char.of_int (i + Char.to_int '1' - 1) with 
    | None -> failwith "impossible" 
    | Some c -> c in 

  let rec flatten lst = 
    match lst with 
    | [] -> []
    | x :: xs -> x @ (flatten xs) in 
  
  let rec split_by_char i line = 
    if i = 9 then String.split_on_chars ~on:['9'] line
    else (
      let result = split_by_char (i + 1) line in 
      let split_by_i = List.map ~f:(String.split_on_chars ~on:[chr i]) result in 
      flatten split_by_i
    ) in 
  
  let num_string line i = 
    let suffix = String.suffix line ((String.length line) - i) in 
    let rec has_prefix lst = 
      match lst with 
      | [] -> None 
      | x :: xs -> 
        if String.is_prefix suffix ~prefix:x
        then Some x
        else has_prefix xs in 
    has_prefix ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"] in 
  
  let id x = x in 
  
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
    | _ -> failwith "not possible" in 
  
  let new_get_first_digit line = 
    match String.get line 0 with 
    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as x -> Char.to_int x - Char.to_int '1' + 1
    | _ -> 
    let splits = split_by_char 0 line in 
    match splits with 
    | [] -> failwith "Not possible"
    | x :: _ -> (
      let get_left il acc = 
        match num_string x il with 
        | None -> acc
        | Some _ as x -> x in 
      
      let string_digit = List.fold_right ~f:get_left (List.init (String.length x) ~f:id) ~init:None in 
      match string_digit with 
      | None -> get_first_digit line 
      | Some s -> string_to_digit s
    ) in 
    
  let new_get_last_digit line = 
    match String.get line (String.length line - 1) with 
    | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as x -> Char.to_int x - Char.to_int '1' + 1
    | _ -> 
    let splits = split_by_char 0 line in 
    match List.rev splits with 
    | [] -> failwith "Not possible"
    | x :: _ -> (
      let get_left acc il = 
        match num_string x il with 
        | None -> acc
        | Some _ as x -> x in 
    let string_digit = List.fold_left ~f:get_left ~init:None (List.init (String.length x) ~f:id) in 
      match string_digit with 
      | None -> get_last_digit line 
      | Some s -> string_to_digit s
    ) in 
  
  let new_number line = 
    (new_get_first_digit line) * 10 + (new_get_last_digit line) in 
  
  Int.to_string (List.fold ~init:0 ~f:( + ) (List.map ~f:new_number lines))