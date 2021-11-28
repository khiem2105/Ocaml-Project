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

(* Strategie 1 *)
let generate_polynome (taille : int) : Polynomes.polynome = 
  let lst = Utils.gen_permutation taille in
    let a = Abr.construct lst in 
      let a_labeled = Abr.etiquetage a in 
        let a_grammaire = Abr.gen_arb a_labeled in 
          let p = Grammaire_arbre.arb2poly a_grammaire in p;;

