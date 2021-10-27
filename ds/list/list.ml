(* [length_l l1] returns the length of [l1]*)
let rec length_l l1 = match l1 with
    |[] -> 0
    |e::q -> 1 + length_l q;;

(* [sum_l l1] returns the sum of elements of [l1] *)
let rec sum_l l1 = match l1 with
    | [] -> 0
    | e::q -> e + sum_l q;;
    
(* [mem_l e l1] returns if e belongs to [l1]*)     
let rec mem_l e l1 = match l1 with
    |[] -> false
    |e1::q -> e1 = e || mem_l e q;;
    
(* [append_l l1 l2] returns the concatenation between [l1] and [l2] *)
let rec append_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> e::append_l q l2;;
    
(* [rev_append_l l1 l2] returns the concatenation between the reverse of [l1] and [l2]*)
(* for reverse a list, put [l2] = []*)
let rec rev_append_l l1 l2 = match l1 with
    |[] -> l2
    |e::q -> rev_append_l q (e::l2)

(* [mini_l l1] returns the minimum of [l1]*)
let rec mini_l l1 = match l1 with
    |[] -> max_int
    |e::q -> let m = mini_l q in
            if e < m then e
            else m;;
            
(** [maxi_l l1] returns the maximum of [l1]*)            
let rec maxi_l l1 = match l1 with
    |[] -> min_int
    |e::q -> let m = maxi_l q in
            if e > m then e
            else m;;

(* [increase_l l1] returns if [l1] increases *)
let rec increase_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e>e1 || increase_l (e1::q);;

(* [decrease_l l1] returns if [l1] decreases *)
let rec decrease_l l1 = match l1 with
    |[] -> true
    |[e] -> true
    |e::e1::q -> e<e1 || increase_l (e1::q);;


