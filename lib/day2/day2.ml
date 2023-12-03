open! Base

type information = {
  red : int; 
  green : int; 
  blue : int
}

let reds : int = 12
let greens : int = 13
let blues : int = 14

type information_kind = Red of int | Green of int | Blue of int

type game = information list

let parse_line (line : string) : game = 
  let split = String.split ~on:':' line in 
  match List.nth split 1 with 
  | None -> failwith "impossible"
  | Some s -> 
    let s = String.strip s in 
    let sets = String.split ~on:';' s in 
    let sets = List.map ~f:String.strip sets in 
    let information = List.map ~f:(String.split ~on:',') sets in 
    let information = List.map ~f:(List.map ~f:String.strip) information in 
    let process_information (information : string list) : information = 
      let splits = List.map ~f:(String.split ~on:' ') information in 
      let splits = List.map ~f:(List.map ~f:String.strip) splits in 
      let process_number_of_cubes (n_cubes : string list) : information_kind = 
        match (List.nth n_cubes 1) with 
        | None -> failwith "impossible"
        | Some color -> 
          let n = match List.nth n_cubes 0 with None -> failwith "impossible" | Some n -> Int.of_string n in 
          match color with 
          | "red" -> Red n
          | "green" -> Green n
          | "blue" -> Blue n
          | _ -> failwith "impossible" in 
      let information_kinds = List.map ~f:process_number_of_cubes splits in 
      let accumulate acc information_kind = 
        match information_kind with 
        | Red n -> {acc with red = acc.red + n}
        | Green n -> {acc with green = acc.green + n}
        | Blue n -> {acc with blue = acc.blue + n} in 
      List.fold ~init:{red = 0; green = 0; blue = 0} ~f:accumulate information_kinds in 
    List.map ~f:process_information information

let problem1 lines = 
  let process_line line = parse_line (String.strip line) in 
  let games : game list = List.map ~f:process_line lines in 
  let process_id i game = (i + 1, game) in 
  let games_with_ids : (int * game) list = List.mapi ~f:process_id games in 
  let works game = 
    game.red <= reds && game.blue <= blues && game.green <= greens in 

  let rec accum acc (id, games) = 
    match games with
    | [] -> 
      id + acc 
    | game :: games -> 
      if works game then accum acc (id, games)
      else acc in 
  Int.to_string (List.fold ~f:accum ~init:0 games_with_ids)
  

let problem2 lines = 
  let process_line line = parse_line (String.strip line) in 
  let games : game list = List.map ~f:process_line lines in 
  let power game = 
    let accum acc info = 
      {
        red = Int.max acc.red info.red; 
        green = Int.max acc.green info.green; 
        blue = Int.max acc.blue info.blue
      } in 
    let info = List.fold ~init:{red = 0; green = 0; blue = 0} ~f:accum game in 
    info.red * info.blue * info.green in 
  let power_of_games = List.map ~f:power games in 
  Int.to_string (List.fold ~init:0 ~f:( + ) power_of_games)