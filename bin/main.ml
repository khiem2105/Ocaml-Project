(* open Polynomes;;
open Utils;;
open Grammaire_arbre;;
open Abr;; *)
open Lib;;
open Polynomes;;
open Utils;;
open Abr;;
open Grammaire_arbre;;


(* let lst_1 = Utils.gen_permutation (2*2*2*2*2*2*2*2*2*2*2*2*2) and lst_2 = Utils.gen_permutation (2*2*2*2*2*2*2*2*2*2*2*2*2) in 
  let a1 = Abr.construct lst_1 and a2 = Abr.construct lst_2 in 
    let a1_labeled = Abr.etiquetage a1 and a2_labeled = Abr.etiquetage a2 in 
      let a1_grammaire = Abr.gen_arb a1_labeled and a2_grammaire = Abr.gen_arb a2_labeled in 
        let p1 = Grammaire_arbre.arb2poly a1_grammaire and p2 = Grammaire_arbre.arb2poly a2_grammaire in
          Polynomes.print_polynome (Polynomes.poly_add p1 p2) *)

let rec print_numbers oc = function 
  | [] -> ()
  | e::tl -> Printf.fprintf oc "%d %f\n" (fst e) (snd e); print_numbers oc tl



(* Strategie 1 *)
let generate_polynome (taille : int) : Polynomes.polynome = 
  let lst = Utils.gen_permutation taille in
    let a = Abr.construct lst in 
      let a_labeled = Abr.etiquetage a in 
        let a_grammaire = Abr.gen_arb a_labeled in 
          let p = Grammaire_arbre.arb2poly a_grammaire in p;;

let rec add_list_polynome (lst_pols : Polynomes.polynome list) : Polynomes.polynome = 
  match lst_pols with 
  | [] -> []
  | p1 :: ps -> Polynomes.poly_add p1 (add_list_polynome ps);;

let add_list_polynome_tail_recursive (lst_pols : Polynomes.polynome list) : Polynomes.polynome =
  let rec add_acc acc lst = match lst with
  | [] -> acc
  | p1 :: ps -> add_acc (Polynomes.poly_add acc p1) ps
  in add_acc [] lst_pols;;

let rec mul_list_polynome (lst_pols : Polynomes.polynome list) : Polynomes.polynome = 
  match lst_pols with 
  | [] -> [{coeff = 1; degre = 0}]
  | p1 :: ps -> Polynomes.poly_prod p1 (mul_list_polynome ps);;

let mul_list_polynome_tail_recursive (lst_pols : Polynomes.polynome list) : Polynomes.polynome =
  let rec mul_acc acc lst = match lst with
  | [] -> acc
  | p1 :: ps -> mul_acc (Polynomes.poly_prod acc p1) ps
  in mul_acc [{coeff = 1; degre = 0}] lst_pols

let construct_list_polynomes (taille_lst : int) (taille_polynome : int) : Polynomes.polynome list = 
  let rec aux count lst = match count with
  | 0 -> lst
  | _ -> aux (count - 1) (generate_polynome taille_polynome :: lst)
  in aux taille_lst [];;

let n = ref 100 and lst = ref [] and lst_time = ref [] in 
  while !n <= 1000 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = List.fold_left (fun acc pol -> Polynomes.poly_add acc pol) [] l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 10
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data1.txt" in
  print_numbers oc data;;

let n = ref 100 and lst = ref [] and lst_time = ref [] in 
  while !n <= 1000 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = add_list_polynome l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 10
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data2.txt" in
  print_numbers oc data;;

let n = ref 100 and lst = ref [] and lst_time = ref [] in 
  while !n <= 1000 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = add_list_polynome_tail_recursive l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 10
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data3.txt" in
  print_numbers oc data;;

let n = ref 10 and lst = ref [] and lst_time = ref [] in 
  while !n <= 50 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = List.fold_left (fun acc pol -> Polynomes.poly_prod acc pol) [{coeff = 1; degre = 0}] l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 1
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data4.txt" in
  print_numbers oc data;;

let n = ref 10 and lst = ref [] and lst_time = ref [] in 
  while !n <= 50 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = mul_list_polynome l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 1
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data5.txt" in
  print_numbers oc data;;

let n = ref 10 and lst = ref [] and lst_time = ref [] in 
  while !n <= 50 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = mul_list_polynome_tail_recursive l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 1
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data6.txt" in
  print_numbers oc data;;

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n)
    in let _ = List.fold_left (fun acc pol -> Polynomes.poly_add acc pol) [] l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data7.txt" in
  print_numbers oc data;; 

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n)
    in let _ = add_list_polynome l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data8.txt" in
  print_numbers oc data;;

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n)
    in let _ = add_list_polynome_tail_recursive l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data9.txt" in
  print_numbers oc data;;

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n) 
    in let _ = List.fold_left (fun acc pol -> Polynomes.poly_prod acc pol) [{coeff = 1; degre = 0}] l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data10.txt" in
  print_numbers oc data;;

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n) 
    in let _ = mul_list_polynome l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data11.txt" in
  print_numbers oc data;;

let n = ref 0. and lst = ref [] and lst_time = ref [] in 
  while !n <= 13. do 
    lst := int_of_float (2. ** !n) :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes 15 (2 * int_of_float !n) 
    in let _ = mul_list_polynome_tail_recursive l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n +. 1.
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data12.txt" in
  print_numbers oc data;;
