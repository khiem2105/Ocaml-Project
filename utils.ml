open Random;;
Random.self_init ();;
module Utils =
struct

(*Question 1.8*)
  let extraction_alea l p = 
    let len = (List.length l) in 
    let r = (Random.int len) in
    let rec aux c l1 l2 = 
      match c, l1 with
      | _, [] -> (l, p)
      | 0, h::t -> (List.rev_append l2 t, h::p)
      | _, h::t -> aux (c - 1) t (h::l2)
    in aux r l [];;

(*Question 1.9*)
  let rec shuffle (l, p) =
    match l with 
    | [] -> p
    | _ -> shuffle (extraction_alea l p);;

  let gen_permutation n = 
    let rec aux count list =
      match count with
      | 0 -> shuffle  (list, [])
      | _ -> aux (count - 1) (count::list)
    in aux n [];; 
end;;


