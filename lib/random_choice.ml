module Random_choice = 
struct
  exception EmptyList;;
  let rec random_choice (l : ('a * float) list) (k : float) : 'a = match l with
    | [] -> raise EmptyList
    | (v, p) :: t -> if k < p then v else random_choice t (k -.p)

end;;