module Polynomes =
struct
(**I.1 Partie Polynômes sous forme linéaire**)

(**Question 1.1**)
type monome = {coeff : int; degre : int};;
type polynome = monome list;;

(**Fonctions d'affichage**)
(*Afficher la puissance*)
let print_pow (d : int) = match d with 
 | 1 -> Printf.printf "x"
 | _ -> Printf.printf "x^%d" d
;;

(*Afficher le coefficient*) 
let print_coeff (tete : bool) (c : int) = match tete, c with
	|true, 1 -> () 
	|false, 1 -> Printf.printf "+%d" c
	|true, _ -> Printf.printf "%d" c
	|false, _ -> if c > 0 then Printf.printf "+%d" c else Printf.printf "%d" c;;

(*Afficher le monôme*)
let print_monome (m : monome) (tete : bool) = match m with
 | {degre=0} -> print_coeff  tete m.coeff
 | _ -> print_coeff tete m.coeff; print_pow m.degre;;

(*Afficher un polynôme*) 
let rec print_polynome_aux (tete : bool) (p : polynome) = match p with 
	| [] -> ()
	| t::q -> print_monome t tete; print_polynome_aux false q

let print_polynome (p : polynome) = match p with 
	| [] -> Printf.printf "0"
	| _ -> print_polynome_aux true p;;

(*Fonction intermédiaire pour les monômes*)
let addition_monomes (ma : monome) (mb : monome) : monome = 
	if ma.degre <> mb.degre then failwith "Monomes de degré différent, Addition impossible\n"
        else {coeff=ma.coeff+mb.coeff; degre=ma.degre};;

let produit_monomes (ma : monome) (mb : monome) : monome = 
	{coeff=ma.coeff*mb.coeff ; degre=ma.degre+mb.degre};;

(**Question 1.2**)
let rec inserer_monome (m:monome) (p:polynome) = match p with 
 | [] -> m::[]
 | t::q ->
    (*Si le monome a un degré supérieur à celui de l'élément en tête de liste on l'insère en tête*)
 	if m.degre > t.degre then m::p 
 	(*Si le monome a un degré équivalent à celui de l'élément en tête de liste on les additionne et insère le monome resultant en tête*)
 	else if m.degre = t.degre then (addition_monomes m t)::q
 	(*Si non, on appelle la fonction de recursive sur la queue*)
 	else t::(inserer_monome m q);;

let rec canonique (p:polynome) = match p with 
 | [] -> []
 | t::q -> inserer_monome t (canonique q);;

(**Question 1.3**)
let rec poly_add (p1 : polynome) (p2 : polynome) : polynome = match p1, p2 with
	|p1,[]-> p1
	|[],p2 -> p2
	|({degre=d1} as h1)::t1, ({degre=d2} as h2)::t2 ->
		if d1 < d2 
			then h1::poly_add t1 p2
		else if d2 < d1
			then h2::poly_add p1 t2
		else
			let c = h1.coeff+h2.coeff in
				if c = 0 
					then poly_add t1 t2
				else
					{coeff=c; degre=d1}::poly_add t1 t2;;


(**Question 1.4**)
let produit_monome_polynome (m : monome) (p : polynome) = List.map (fun mp -> produit_monomes m mp) p;;

let poly_prod (p1 : polynome) (p2 : polynome) =
  List.fold_left
    (fun acc m1 -> poly_add acc (produit_monome_polynome m1 p2))
    [] p1;;

(**I.2 Expression Arborescente**)
end;;




