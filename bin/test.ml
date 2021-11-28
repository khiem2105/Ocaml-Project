open Lib;;
open Polynomes;;
open Utils;;
open Abr;;
open Grammaire_arbre;;

(* let rec print_numbers oc = function 
  | [] -> ()
  | e::tl -> Printf.fprintf oc "%d %f\n" (fst e) (snd e); print_numbers oc tl;; *)

let generate_polynome (taille : int) : Polynomes.polynome = 
  let lst = Utils.gen_permutation taille in
    let a = Abr.construct lst in 
      let a_labeled = Abr.etiquetage a in 
        let a_grammaire = Abr.gen_arb a_labeled in 
          let p = Grammaire_arbre.arb2poly a_grammaire in p;;

let construct_list_polynomes (taille_lst : int) (taille_polynome : int) : Polynomes.polynome list = 
  let rec aux count lst = match count with
  | 0 -> lst
  | _ -> aux (count - 1) (generate_polynome taille_polynome :: lst)
  in aux taille_lst [];;

(* let n = ref 100 and lst = ref [] and lst_time = ref [] in 
  while !n <= 1000 do 
    lst := !n :: !lst;
    let start = Unix.gettimeofday () 
    in let l = construct_list_polynomes !n 20 
    in let _ = List.fold_left (fun acc pol -> Polynomes.poly_prod acc pol) [{coeff = 1; degre = 0}] l
    in let stop = Unix.gettimeofday ()
    in let t = stop -. start
    in lst_time := t :: !lst_time;
    n := !n + 10
  done;
  let data = List.combine !lst !lst_time in 
  let oc = open_out "data/data4.txt" in
  print_numbers oc data;; *)

let l = construct_list_polynomes 100 20
in let p = List.fold_left (fun acc pol -> Polynomes.poly_prod acc pol) [{coeff = 1; degre = 0}] l
in Polynomes.print_polynome p