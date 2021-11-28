open Polynomes;;

module Grammaire_arbre = 
struct
  exception GrammaireNotVerified;;

  type operator = Mul | Plus | Pow;;

  (* Arbre qui vérifie la grammaire *)
  type grammaire_arbre = 
    | Empty
    | Pow_Node of {op : operator; var : string; degre : int}
    | Mul_Or_Plus_Node of {op : operator; sous_arbres : grammaire_arbre list}
    | Int_Node of {label : int}

  let rec arb2poly (g_a : grammaire_arbre) : Polynomes.polynome = match g_a with
    | Empty -> []
    | Int_Node{label = x} -> [{coeff = x; degre = 0}]
    | Pow_Node{op = Pow; var = _; degre = d} -> [{coeff = 1; degre = d}]
    | Mul_Or_Plus_Node{op = Plus; sous_arbres = lst_arbre} -> List.fold_left (fun acc sous_g_a -> Polynomes.poly_add acc (arb2poly sous_g_a)) [] lst_arbre
    | Mul_Or_Plus_Node{op = Mul; sous_arbres = lst_arbre} -> List.fold_left (fun acc sous_g_a -> Polynomes.poly_prod acc (arb2poly sous_g_a)) [{coeff = 1; degre = 0}] lst_arbre
    | _ -> raise GrammaireNotVerified

end;;