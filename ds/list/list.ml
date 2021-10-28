
let fst_l l1 = match l1 with
    |[] -> failwith " not elements"
    |e::q -> e;;
    

let rec lst_l l1 = match l1 with
    |[] -> failwith "not elements"
    |[e] -> e
    |e::q -> lst_l q;;


let rec nth_l e l1 = match l1 with
    |[] -> failwith "impossible"
    |e1::q -> if e = 0 then e1
                else nth_l (e-1) q;;

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
    
(* [equal_l [a1; ...; an] [b1; ..; bm]] holds when the two input lists have the same length, and for each pair of elements ai, bi at the same position we have eq ai bi.*)
let rec equal_l l1 l2 = match l1,l2 with
    |[],[] -> true
    |[],_ | _,[]-> false
    |e::q, e1::q1 -> if e <> e1 then false 
                    else equal_l q q1;;
                    
                    
let rec map_l f l1 = match l1 with
    | [] -> []
    |e::q -> f(e)::map_l f q;;


let rec filter_l f l1 = match l1 with
    | [] -> []
    |e::q -> if f(e) then e::filter_l f q
            else filter_l f q;;


let rec for_all_l f l1 = match l1 with
    |[] -> true
    |e::q -> if f(e) = true then for_all_l f q
            else false;;
            

let rec exists_l f l1 = match l1 with
    |[] -> false
    |e::q ->  f(e) || exists_l f q
    
    
let rec combine_pairs_l l1 l2 = match l1, l2 with
    |[],[] -> []
    |_,[] | [],_ -> failwith "Invalid_argument"
    |e1::q1, e2::q2 -> (e1,e2)::combine_pairs_l q1 q2;;


let rec split_pairs_l l1 = match l1 with
    |[] -> ([],[])
    |(e1,e2)::q -> let q1,q2 = split_pairs_l q in
                    e1::q1, e2::q2;;


let rec split_l l1 = match l1 with
    |[] -> ([],[])
    |[e] -> ([e],[])
    |e1::e2::q -> let q1, q2 = split_l q in
                    e1::q1, e2::q2;;
                    

let rec fusion_l l1 l2 = match l1, l2 with
    |[],_ -> l2
    |_,[] -> l1
    |e1::q1, e2::q2 -> if e1< e2 then e1::fusion_l q1 l2
                        else e2::fusion_l l1 q2;;
                        

let rec tri_l l1 = match l1 with
    |[] -> []
    |[e] -> [e]
    |_ -> let l2, l3 = split_l l1 in
        fusion_l (tri_l l2) (tri_l l3);;
