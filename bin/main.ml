open! Base
open Stdlib

open Adventofcode2023.Input

let day = 1
let first_practice_lines = get_lines (Practice (day, 1))
let second_practice_lines = get_lines (Practice (day, 2))

let final_lines = get_lines (Final 1)

let () = Printf.printf "Advent of Code Day %d Solutions\n" day

let () = print_endline "Printing Solutions for the first star"

let () = print_endline "Solution to the practice"
let solution = Day1.problem1 first_practice_lines

let () = Printf.printf "%s\n" solution

let () = print_endline "Solution to the final"

let solution = Day1.problem1 final_lines
let () = Printf.printf "%s\n" solution

let () = print_endline "-----------------------------"

let () = print_endline "Printing Solutions for the second star"

let () = print_endline "Solution to the practice"
let solution = Day1.problem2 second_practice_lines

let () = Printf.printf "%s\n" solution

let () = print_endline "Solution to the final"

let solution = Day1.problem2 final_lines

let () = Printf.printf "%s\n" solution