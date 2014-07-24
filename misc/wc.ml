#require "core"
open Core.Std


       
let test_file = "./test.txt"
	  
let count_lines file =
  List.length (In_channel.read_lines file);;
  
  

(*************************************)
(* Somme les entiers dans une liste. *)
(*************************************)
	      
  let l_int = [1;2;5;4;8;7]

(* Fonction recursive non terminale *)
  let rec sum_int_list = function
    | [] -> 0
    | hd :: tl -> hd + sum_int_list tl
  ;;
    
		 
  (* Fonction recursive terminale *)
  let sum_int_list_tail l =
    let rec aux accum = function
      | [] -> accum
      | hd :: tl -> aux (accum + hd) tl
    in
    aux 0 l
  ;;
    
    (* Version avec List.fold *)  
    List.fold ~init:0 ~f:(+) l_int;;
    

(********************************************************)
(* Function qui compte les éléments dans un fichier     *)
(********************************************************)
      

    (* Function générique qui compte les éléments comptés par "fn" *)
    let count_in_file file fn =
      let file_list = In_channel.read_lines file in
      List.fold ~init:0 ~f:(fun accum current -> accum + fn current) file_list;;
      
      
    (* Function de comptage des charactères *)
    let nb_char str =
      String.length str;;
      
    (* Function de comptage des mots *)
    let nb_word str =
      List.length (String.split_on_chars str ~on:[' ';'\t']);;
      
    (* count_in_file test_file nb_char;; *)
    (* count_in_file test_file nb_word;; *)
      
    let print_result filename nb_line nb_word nb_char = 
      Printf.printf 
	"Dans le fichier %s : \n   -%d ligne(s)\n   -%d mot(s)\n   -%d charactère(s)\n"
	filename
	nb_line
	nb_word
	nb_char
    ;;


    let main () =
      let arg   = Sys.argv in
      let l_arg = Array.length arg in
      match l_arg with
      | 1 -> failwith "Comment veux-tu que je compte ce qu'il y a dans un fichier, si tu me donnes pas un nom de fichier..."
      | 2 -> let filename = Array.get Sys.argv 1 in
	     (match Sys.file_exists filename with
	      | `Yes -> let nb_line = count_lines filename in
			let nb_char = count_in_file filename nb_char in
			let nb_word = count_in_file filename nb_word in
			print_result filename nb_line nb_word nb_char
	      | `No -> failwith "Tant qu'à faire, plutôt un fichier qui existe ;)"
	      | `Unknown -> failwith "Je n'arrive même pas à savoir si il existe ou non...")
      | _ -> failwith "Il me faut qu'un seul argument...";;

      if !Sys.interactive then () else main ();;
      

