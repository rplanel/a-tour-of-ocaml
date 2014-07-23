#require "core"
open Core.Std

let count_lines file =
  List.length (In_channel.read_lines file);;

count_lines "test.txt";;


(* let rec  sum_list list = *)
(*   let sum = List.map (fun x -> sum + x) list ;; *)


(* let count_characters file *)
