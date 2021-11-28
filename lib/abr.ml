open Random_choice;;
open Grammaire_arbre;;

Random.self_init ();;

module Abr =
struct
  exception  NoRule;;

(*Creation du type ABR*)
  type 'a binary_tree =
    | Empty
    | Node of {label :'a; left : 'a binary_tree; right : 'a binary_tree};;

(*Fonctions utilitaires aux ABR*)
  let rec insert (tree : int binary_tree) (n : int) : int binary_tree =
    match tree with
    | Empty -> Node{label = n; left = Empty; right = Empty}
    | Node{label = x; left = l; right = r} ->
      if n = x then
        tree
      else if n < x then
        Node{label = x; left = insert l n; right = r}
      else
        Node{label = x; left = l; right = insert r n}

  let construct (list : int list) : int binary_tree = 
    let rec aux l bt = 
      match l with
      | [] -> bt
      | h::t -> aux t (insert bt h)
    in aux list Empty

  let rec search (tree : int binary_tree) (n : int) : bool =
    match tree with
    | Empty -> false
    | Node{label = x; left = l; right = r} -> 
      if n = x then
        true
      else if n < x then
        search l n
      else 
        search r n

  let rec etiquetage ?operator_list:(op_lst=[("+", 0.75); ("*", 0.25)]) (tree : int binary_tree) : string binary_tree =
    match tree with
    | Empty -> Empty

    | Node{label = l; left = Empty; right = Empty} ->
      if l mod 2 = 1 then
        let e = Random.int 400 - 200 in
          Node{label = "*"; left = Node{label = string_of_int e; left = Empty; right = Empty}; right = Node{label = "x"; left = Empty; right = Empty}}
      else
        let e = Random.int 100 in
          Node{label = "^"; left = Node{label = "x"; left = Empty; right = Empty}; right = Node{label = string_of_int e; left = Empty; right = Empty}}

    | Node{label = _; left = left_tree; right = Empty} ->
      let op = Random_choice.random_choice op_lst (Random.float 1.) in 
        let e = Random.int 400 - 200 in
          let lst = [(string_of_int e, 0.5); ("x", 0.5)] in
            Node{label = op; left = etiquetage left_tree; right = Node{label = Random_choice.random_choice lst (Random.float 1.); left = Empty; right = Empty}}

    | Node{label = _; left = Empty; right = right_tree} ->
      let op = Random_choice.random_choice op_lst (Random.float 1.) in 
        let e = Random.int 400 - 200 in
          let lst = [(string_of_int e, 0.5); ("x", 0.5)] in
            Node{label = op; left = Node{label = Random_choice.random_choice lst (Random.float 1.); left = Empty; right = Empty}; right = etiquetage right_tree}
    
    | Node{label = _; left = left_tree; right = right_tree} ->
      let op = Random_choice.random_choice op_lst (Random.float 1.) in 
        Node{label = op; left = etiquetage left_tree; right = etiquetage right_tree}

  let rec gen_arb (tree : string binary_tree) : Grammaire_arbre.grammaire_arbre = match tree with
    | Empty -> Grammaire_arbre.Empty

    | Node{label = x; left = Empty; right = Empty} ->
      if x = "x" then
        Grammaire_arbre.Pow_Node{op = Pow; var = "x"; degre = 1}
      else
        Grammaire_arbre.Int_Node{label = int_of_string x}

    | Node{label = "^"; left = _; right = Node{label = d; left = _; right = _}} ->
      Grammaire_arbre.Pow_Node{op = Pow; var = "x"; degre = int_of_string d}

    | Node{label = "+"; left = Node{label = "+"; left = _; right = _} as l; right = Node{label = "+"; left = _; right = _} as r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = a1}, Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = a2} ->
              Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = List.rev_append a1 a2}
            | _, _ -> raise NoRule
        )
      
    | Node{label = "+"; left = Node{label = "+"; left = _; right = _} as l; right = r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = a1}, _ ->
              Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = new_right :: a1}
            | _, _ -> raise NoRule
        )
  
    | Node{label = "+"; left = l; right = Node{label = "+"; left = _; right = _} as r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | _, Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = a2} ->
              Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = new_left :: a2}
            | _, _ -> raise NoRule
        )
  
    | Node{label = "+"; left = l; right = r} ->
        let new_left = gen_arb l and new_right = gen_arb r in
          Grammaire_arbre.Mul_Or_Plus_Node{op = Plus; sous_arbres = [new_left; new_right]}
      
    | Node{label = "*"; left = Node{label = "*"; left = _; right = _} as l; right = Node{label = "*"; left = _; right = _} as r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = a1}, Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = a2} ->
              Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = List.rev_append a1 a2}
            | _, _ -> raise NoRule
        )
    
    | Node{label = "*"; left = Node{label = "*"; left = _; right = _} as l; right = r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = a1}, _ ->
                  Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = new_right :: a1}
            | _, _ -> raise NoRule
        )

    | Node{label = "*"; left = l; right = Node{label = "*"; left = _; right = _} as r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        (
          match new_left, new_right with
            | _, Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = a2} ->
              Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = new_left :: a2}
            | _, _ -> raise NoRule
        )

    | Node{label = "*"; left = l; right = r} ->
      let new_left = gen_arb l and new_right = gen_arb r in
        Grammaire_arbre.Mul_Or_Plus_Node{op = Mul; sous_arbres = [new_left; new_right]}

    | _ -> raise NoRule

end;;