open Random;;
Random.self_init ();;
module ABR =
struct

(**Question 1.10**)

(*Creation du type ABR*)
  type 'a binary_tree =
    | Empty
    | Node of 'a * 'a binary_tree * 'a binary_tree;;

(*Fonctions utilitaires aux ABR*)
  let rec insert (tree : int binary_tree) (n : int) : int binary_tree =
    match tree with
    | Empty -> (Node (n, Empty, Empty));
    | (Node (x, left, right)) -> 
      if n = x then
        tree
      else if n < x then
        (Node (x, (insert left n), right))
      else 
        (Node (x, left, (insert right n)))

  let construct (list : int list) : int binary_tree = 
    let rec aux l bt = 
      match l with
      | [] -> bt
      | h::t -> aux t (insert bt h)
    in aux list Empty

  let rec search (tree : int binary_tree) (n : int) : bool =
    match tree with
    | Empty -> false
    | (Node (x, left, right)) -> 
      if n = x then
        true
      else if n < x then
        search left n
      else 
        search right n

end;;