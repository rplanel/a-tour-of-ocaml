let square x =
  x *. x;;


(* Printf.printf "square 60 = %f\n"  (square 60.);; *)

let abs x =
  if x < 0. then -.x
  else x;;


let mean a b =
  (a +. b) /. 2.;;


let geom_mean a b =
  sqrt (a *. b);;

(* print_string "geom_mean 5. 2. = ";; *)
(* print_float (geom_mean 5. 2.);; *)
(* print_newline ();; *)

let identity x = x;;



let eval f x =
  f x;;

(**************************************************************************************)
(* val eval : ('a -> 'b) -> 'a -> 'b = <fun>                                          *)
(* les types sont :                                                                   *)
(*                                                                                    *)
(* la fonction eval prend une fonction qui elle-même prend                            *)
(* un argument de type générique 'a et retourne un "autre" type générique 'b          *)
(* (même si 'a peut être de même type que 'b :) ) cela correspond à : "('a -> 'b) ->" *)
(*                                                                                    *)
(* Ceci : "('a -> 'b) ->" nous retourne une fonction qui prend un argument            *)
(* de type 'a et retourne une valeur de type 'b. "-> 'a -> 'b"                        *)
(**************************************************************************************)


let compose f g x =
  g(f x);; (* g o f *)


(***************************************************************************)
(* val compose : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c = <fun> :            *)
(* -------------------------------------------------------------           *)
(* compose est une fonction qui prend comme paramètre :                    *)
(*   -une fonction "('a -> 'b)"                                            *)
(*    (qui prend une type generique 'a qui retourne un type générique 'b ) *)
(* et retourne                                                             *)
(*   -une fonction " -> ('b -> 'c)"                                        *)
(*     ( qui prend comme paramètre une fonction "('b -> 'c)"               *)
(*         ( qui prend comme paramètre un argument de type generique 'b    *)
(*           et retourne un type générique 'c )                            *)
(*     )                                                                   *)
(* et retourne                                                             *)
(*    -une fonction " -> 'a -> 'c "                                        *)
(*      qui prend comme paramètre un argument de type 'a                   *)
(* et retourne un valeur de type 'b                                        *)
(***************************************************************************)




(***************************)
(* Functions on 2d vectors *)
(***************************)

let origin = (0.,0.);;
let ex     = (1.,0.);;
let ey     = (0.,1.);;

let norm2 (x,y) =
  sqrt (x**2. +. y**2.);;

let (+|) (x1,y1) (x2,y2) =
  ( x1 +. x2, y1 +. y2 );;

let ( *| ) lambda (x,y) =
  ( lambda *. x , lambda *. y );;

(* print_string "norm2 (ex +| 2. *| ey) = ";; *)
(* print_float (norm2 (ex +| 2. *| ey));; *)
(* print_newline ();; *)



(******************************)
(* First steps with recursion *)
(******************************)

let rec sigma n = 
  if n = 0 then 0 else n + sigma (n-1);;


let rec sigma n = 
  if n < 0 then failwith ("chuis pas capab !!") 
  else if n = 0 then 0
  else n + sigma (n-1);;


let rec fact n =
  if n < 0 then failwith "NUL!!"
  else if n = 0 then 1
  else n * fact (n-1);;


let rec power n f x =
  if n < 0 then failwith (" est le résulat de votre test de personnalité !! :)")
  else if n = 0 then identity x
  else if n = 1 then f x
  else    f (power (n-1) f x);;

(* print_string "power 2 fact 3 = ";; *)
(* print_int (power 2 fact 3);; *)
(* print_int (power (-5) fact 3);; *)
(* print_newline();; *)

(******************************************)
(* Application: derivatives of a function *)
(******************************************)



let derivative eps f  x =
  (f ( x +. eps) -. f x) /. eps;;

let square x = x ** 2.;;

(* # derivative 0.001 square 2.;; *)
(* - : float = 4.00099999999969924 *)

(* # derivative 0.001 square 4.;; *)
(* - : float = 8.0010000000037 *)


let relative_error f_prim_approx f_prim x =
  abs (1. -. (f_prim_approx x /. f_prim x));;

let eps            = 0.0001;;
let x_deriv        = 4.;;
let deriv_square x = 2. *. x;;

(* print_newline();; *)
(* print_string "======= square ======";; *)
(* print_newline();; *)
(* print_string "derivative ";; *)
(* print_float eps;; *)
(* print_string " square ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)
(* print_float (derivative eps square x_deriv);; *)
(* print_newline();; *)

(* print_string "deriv_square ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)
(* print_float (deriv_square x_deriv);; *)
(* print_newline();; *)


(* print_string "Relative error of derivative ";; *)
(* print_float eps;; *)
(* print_string " square ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)
(* print_float (relative_error (derivative eps square) deriv_square x_deriv);; *)
(* print_newline();; *)

(* print_newline();; *)
(* print_string "======= log ======";; *)
(* print_newline();; *)
(* print_string "this is the approx of second derivative ";; *)
(* print_float eps;; *)
(* print_string " log ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)

(* print_float  (power 2 (derivative eps) log x_deriv);; *)
(* print_newline();; *)
  

let log_prim_second x = (-1.) /. x**2.;;

(* print_string "this is the second derivative ";; *)
(* print_float eps;; *)
(* print_string " log ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)
(* print_float (log_prim_second x_deriv);; *)
(* print_newline();; *)

(* print_string "Relative error of second derivative ";; *)
(* print_float eps;; *)
(* print_string " log ";; *)
(* print_float x_deriv;; *)
(* print_string " = ";; *)
(* print_float (relative_error (power 2 (derivative eps) log) log_prim_second x_deriv);; *)
(* print_newline();; *)



(*****************************************************)
(* Application: computing a root square by dichotomy *)
(*****************************************************)

(**************)
(* Sans print *)
(**************)

let dichotomic_square_root eps x =
  let a = 0. in
  let b = x  in
  let rec aux a b =
    let m = mean a b in
    let m_square = m ** 2. in
    (* La bonne vieille condition de sortie *)
    if m_square = x then (m,m)
    else if b -. a < eps then (a,b)
    else if  m_square > x then aux a m else aux m b
  in
  aux a b ;;


(* Avec print *)

let dichotomic_square_root_with_print eps x =
  let a = 0. in
  let b = x  in
  let rec aux a b =
    let () = Printf.printf "Current value of the interval: (%f,%f)\n" a b in
    let m = mean a b in
    let m_square = m ** 2. in
    (* let () = Printf.printf "Mean = (%f)\n" m in *)
    (* La bonne vieille condition de sortie *)
    if m_square = x then (m,m)
    else if (b -. a) < eps then (a,b)
    else if (m_square) > x then aux a m else aux m b
  in
  aux a b ;;

(***********************************************************)
(* Application: Babylonian method to compute a root square *)
(***********************************************************)

let babylonionan_square_root nb_round s =
  (* Estimation pourrie... mais facile :) *)
  let x0 = s /. 2. in
  let rec aux xn n =
    if n = nb_round then xn
    else
      let () = Printf.printf "x%d = %f\n" n xn in
      let xn_plus_1 = 1. /. 2. *. (xn +. s /. xn) in
      aux xn_plus_1 (n+1)
  in
  aux x0 0;;


(*******************************************************)
(* # dichotomic_square_root_with_print 0.001 96.;;     *)
(* Current value of the interval: (0.000000,96.000000) *)
(* Current value of the interval: (0.000000,48.000000) *)
(* Current value of the interval: (0.000000,24.000000) *)
(* Current value of the interval: (0.000000,12.000000) *)
(* Current value of the interval: (6.000000,12.000000) *)
(* Current value of the interval: (9.000000,12.000000) *)
(* Current value of the interval: (9.000000,10.500000) *)
(* Current value of the interval: (9.750000,10.500000) *)
(* Current value of the interval: (9.750000,10.125000) *)
(* Current value of the interval: (9.750000,9.937500)  *)
(* Current value of the interval: (9.750000,9.843750)  *)
(* Current value of the interval: (9.796875,9.843750)  *)
(* Current value of the interval: (9.796875,9.820312)  *)
(* Current value of the interval: (9.796875,9.808594)  *)
(* Current value of the interval: (9.796875,9.802734)  *)
(* Current value of the interval: (9.796875,9.799805)  *)
(* Current value of the interval: (9.796875,9.798340)  *)
(* Current value of the interval: (9.797607,9.798340)  *)
(* - : float * float = (9.797607421875, 9.79833984375) *)
(*******************************************************)


(*************************************)
(* babylonionan_square_root 10 96.;; *)
(* x0 = 48.000000                    *)
(* x1 = 25.000000                    *)
(* x2 = 14.420000                    *)
(* x3 = 10.538710                    *)
(* x4 = 9.823992                     *)
(* x5 = 9.797993                     *)
(* x6 = 9.797959                     *)
(* x7 = 9.797959                     *)
(* x8 = 9.797959                     *)
(* x9 = 9.797959                     *)
(* - : float = 9.79795897113271153   *)
(*************************************)


(* Les Babyloniens sont les plus forts !!! *)
