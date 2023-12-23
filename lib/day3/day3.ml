open! Base

(* represent coordinates with a pair of ints *)
type coords = int * int

(* we will distinguish between the kind of coordinate *)
type left_coords = coords
type right_coords = coords

module OrderedCoords = 
struct 
  type t = coords * coords
  let compare ((r1, c1), (r1', c1')) ((r2, c2), (r2', c2')) = 
    match Stdlib.compare r1 r2 with 
    | 0 -> (
        match Stdlib.compare r1' r2' with 
        | 0 -> (
          match Stdlib.compare c1 c2 with 
          | 0 -> Stdlib.compare c1' c2'
          | c -> c
        )
        | c -> c
      )
    | c -> c
end
module CoordsMap = Stdlib.Map.Make(OrderedCoords)

(* this is the trailing zeroes *)
type power = int

(* a cell can either be a number with the corresponding left and right 
   endpoints, and the place value of the most significant digit expressed 
   as the power, or it is not a number *)
type cell = Number of int * left_coords * right_coords * power | Nan
[@@derive sexp_of]

let part_numbers lines = 
  let rows = List.length lines in 
  match List.nth lines 0 with 
  | None -> failwith "no"
  | Some line -> 
  let cols = String.length line in 
  let arr = Array.make_matrix ~dimx:rows ~dimy:cols 'a' in 
  let update row line = 
    let update col c = 
      arr.(row).(col) <- c in 
    String.iteri ~f:update line in 

  let () = List.iteri ~f:update lines in 
  let in_bounds row col = 
    0 <= row && row < rows && 0 <= col && col < cols in
  
  let drows = [-1; 0; 1] in 
  let dcols = [-1; 0; 1] in 

  let get_index i line = 
    let get_index j c = 
      ((i, j), c) in 
    Array.mapi ~f:get_index line in 

  let indexed_arr = Array.mapi ~f:get_index arr in 
  
  let mapper (coords, c) : cell Core.Deque.t = 
    let q = Core.Deque.create () in 
    let () = 
      if Char.is_alphanum c 
      then Core.Deque.enqueue_front q (Number(Int.of_string (String.of_char c), coords, coords, 10))
      else Core.Deque.enqueue_front q Nan in 
    q in 
    
  let row_mapper row = 
    Array.map ~f:mapper row in 

  let queues = Array.map ~f:row_mapper indexed_arr in 
  let queue_reducer q1 q2 = 
    let right = Option.value_exn (Core.Deque.dequeue_back q1) in 
    let left = Option.value_exn (Core.Deque.dequeue_front q2) in 
    let rec move q2 q1 = 
      if Core.Deque.is_empty q2 then () 
      else 
        let () = Core.Deque.enqueue_back q1 (Option.value_exn (Core.Deque.dequeue_front q2)) in 
        move q2 q1 in 
    match right, left with 
    | Number (n1, (lx1, ly1), (_, _), p1), Number (n2, (_, _), (rx2, ry2), p2) -> (
      let new_value = Number (n1 * p2 + n2, (lx1, ly1), (rx2, ry2), p1 * p2) in 
      let () = Core.Deque.enqueue_back q1 new_value in 
      let () = move q2 q1 in 
      q1
    )
    | ((Number _) as x), Nan -> (
      let () = Core.Deque.enqueue_back q1 x in 
      let () = Core.Deque.enqueue_front q2 Nan in 
      let () = move q2 q1 in 
      q1
    )
    | Nan, (Number _ as x) -> (
      let () = Core.Deque.enqueue_back q1 Nan in 
      let () = Core.Deque.enqueue_front q2 x in 
      let () = move q2 q1 in 
      q1
    ) 
    | Nan, Nan -> (
      let () = Core.Deque.enqueue_back q1 Nan in 
      let () = move q2 q1 in 
      q1
    ) in 
  let row_queues = Array.map ~f:(fun row -> Option.value_exn (Array.reduce ~f:queue_reducer row)) queues in 
  let rec collect_as_list q = 
    match Core.Deque.dequeue_front q with 
    | Some ((Number _) as x) -> x :: (collect_as_list q)
    | Some Nan -> collect_as_list q
    | _ -> [] in 
  let numbers = Array.map ~f:collect_as_list row_queues in
  let numbers = Option.value ~default:[] (Array.reduce ~f:( @ ) numbers) in 
  let drow_dcol_works drow dcol row col = 
    (in_bounds (row + drow) (col + dcol)) && not (Char.is_alphanum (arr.(row + drow).(col + dcol))) && not (Int.equal (Char.compare arr.(row + drow).(col + dcol) '.') 0) in 
  let check_adjacent (row, col) : bool = 
    let all_combinations : (int * int) list = 
      Option.value ~default:[] (
        List.reduce 
          ~f:( @ ) 
          (List.map drows ~f:(fun drow -> List.map dcols ~f:(fun dcol -> (drow, dcol))))
      ) in 
    Option.value ~default:false (List.reduce (List.map ~f:(fun (drow, dcol) -> drow_dcol_works drow dcol row col) all_combinations) ~f:( || )) in 
  let rec part_number (row1, col1) (row2, col2) : bool = 
    if row1 = row2 
    then (
      if col1 > col2 then failwith "Impossible"
      else if col1 = col2 then check_adjacent (row1, col1) 
      else (check_adjacent (row1, col1)) || (part_number (row1, col1 + 1) (row2, col2))
    )
    else failwith "Impossible" in 
  let is_part_number cell = 
    match cell with 
    | Number (_, (row1, col1), (row2, col2), _) -> part_number (row1, col1) (row2, col2)
    | _ -> false in 
  List.filter ~f:is_part_number numbers

let problem1 lines = 
  let extract_value cell = 
    match cell with 
    | Number (v, _, _, _) -> v
    | _ -> failwith "invalid" in 
  let filtered_numbers = List.map ~f:(fun x -> extract_value x) (part_numbers lines) in 
  Int.to_string (List.fold ~init:0 ~f:( + ) filtered_numbers)

let problem2 lines = 
  let part_numbers = part_numbers lines in 
  let drows = [-1; 0; 1] in 
  let dcols = [-1; 0; 1] in 
  let adjacents drow dcol row col = 
    let i = drow + row in 
    let j = dcol + col in 
    let empty : int CoordsMap.t = CoordsMap.empty in 
    let combine acc cell = 
      match cell with 
      | Number (v, (r1, c1), (r1', c1'), _) -> 
        if r1 = i && c1 <= j && j <= c1' then CoordsMap.add ((r1, c1), (r1', c1')) v acc else acc
      | Nan -> acc in 
    List.fold ~init:empty ~f:combine part_numbers in 
  let adjacent_numbers i j = 
    let all_combinations : (int * int) list = 
      Option.value ~default:[] (
        List.reduce 
          ~f:( @ ) 
          (List.map drows ~f:(fun drow -> List.map dcols ~f:(fun dcol -> (drow, dcol))))
      ) in 
    let maps : int CoordsMap.t list = (List.map ~f:(fun (drow, dcol) -> adjacents drow dcol i j) all_combinations) in 
    let f _ x _ = Some x in 
    let adjacents = List.fold ~f:( CoordsMap.union f ) ~init:CoordsMap.empty maps in 
    List.map ~f:snd (CoordsMap.bindings adjacents) in 
    

  let row_map_index i row = 
    let col_map_index j col = 
      if Int.equal (Char.compare col '*') 0 then (
        let adj = adjacent_numbers i j in 
        (*let () = Stdlib.Printf.printf "%s\n" (String.concat ~sep:", " (List.map ~f:Int.to_string adj)) in *)
        if Int.equal (List.length adj) 2 then Option.value_exn (List.reduce ~f:( * ) adj)
        else 0
      )
      else 0 in 
    List.mapi ~f:col_map_index (String.to_list row) in 

  let gears = List.mapi ~f:row_map_index lines in 
  let gears = Option.value ~default:[] (List.reduce ~f:( @ ) gears) in 
  Int.to_string (Option.value_exn (List.reduce ~f:( + ) gears))
