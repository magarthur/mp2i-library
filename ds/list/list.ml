(* [fst_l l1] returns the first element of [l1]*)
let fst_l l1 = match l1 with
    |[] -> failwith " not elements"
    |e::q -> e;;
    
(* [lst_l l1] returns the last element of [l1]*)
let rec lst_l l1 = match l1 with
    |[] -> failwith "not elements"
    |[e] -> e
    |e::q -> lst_l q;;

(* [nth_l l1] returns the n-th element of [l1] *)
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
    
(* [doublon_l l1] returns true or false if the has one or more duplicates *)
let rec doublon_l l1 = match l1 with
    |[] -> false
    |e::q -> if mem_l e q then true
                else doublon_l q;;

(* [doublon2_l l1] returns [l1] without duplicates *)
let rec doublon2_l l1 = match l1 with
    |[] -> []
    |e::q -> if mem_l e q then doublon2_l q
            else e::doublon2_l q;;
    
(* [equal_l [a1; ...; an] [b1; ..; bm]] holds when the two input lists have the same length, and for each pair of elements ai, bi at the same position we have eq ai bi.*)
let rec equal_l l1 l2 = match l1,l2 with
    |[],[] -> true
    |[],_ | _,[]-> false
    |e::q, e1::q1 -> if e <> e1 then false 
                    else equal_l q q1;;
                    
(* [map f [a1; ...; an]] applies function f to a1, ..., an, and builds the list [f a1; ...; f an] *)           
let rec map_l f l1 = match l1 with
    | [] -> []
    |e::q -> f(e)::map_l f q;;

(* [filter_l f l] returns all the elements of the list l that satisfy the predicate f *)
let rec filter_l f l1 = match l1 with
    | [] -> []
    |e::q -> if f(e) then e::filter_l f q
            else filter_l f q;;

(* [for_all_l f [a1; ...; an]] checks if all elements of the list satisfy the predicate f. That is, it returns (f a1) && (f a2) && ... && (f an) for a non-empty list and true if the list is empty. *)
let rec for_all_l f l1 = match l1 with
    |[] -> true
    |e::q -> if f(e) = true then for_all_l f q
            else false;;
            
(* [exists_l f [a1; ...; an]] checks if at least one element of the list satisfies the predicate f. That is, it returns (f a1) || (f a2) || ... || (f an) for a non-empty list and false if the list is empty. *)
let rec exists_l f l1 = match l1 with
    |[] -> false
    |e::q ->  f(e) || exists_l f q
    
(* [combine_pairs_l l1 l2] transform a pair of lists into a list of pairs: combine [a1; ...; an] [b1; ...; bn] is [(a1,b1); ...; (an,bn)]. *)   
let rec combine_pairs_l l1 l2 = match l1, l2 with
    |[],[] -> []
    |_,[] | [],_ -> failwith "Invalid_argument"
    |e1::q1, e2::q2 -> (e1,e2)::combine_pairs_l q1 q2;;

(* [split_pairs_l l1 ] transform a list of pairs into a pair of lists: split [(a1,b1); ...; (an,bn)] is ([a1; ...; an], [b1; ...; bn]) *)
let rec split_pairs_l l1 = match l1 with
    |[] -> ([],[])
    |(e1,e2)::q -> let q1,q2 = split_pairs_l q in
                    e1::q1, e2::q2;;

(* [split_l l1] transform a list into two lists : split [a1;a2;a3;a4;...,an] is [a1;a3...;an], [a2;a4;...;a(n-1)] *)
let rec split_l l1 = match l1 with
    |[] -> ([],[])
    |[e] -> ([e],[])
    |e1::e2::q -> let q1, q2 = split_l q in
                    e1::q1, e2::q2;;
                    
(* [fusion_l l1 l2] merges two sorted lists into one sorted list *)
let rec fusion_l l1 l2 = match l1, l2 with
    |[],_ -> l2
    |_,[] -> l1
    |e1::q1, e2::q2 -> if e1< e2 then e1::fusion_l q1 l2
                        else e2::fusion_l l1 q2;;
                        
(* [tri_l l1] transform l1 into a sorted list *)
let rec sort_l l1 = match l1 with
    |[] -> []
    |[e] -> [e]
    |_ -> let l2, l3 = split_l l1 in
        fusion_l (sort_l l2) (sort_l l3);;
        
(* [add_l e ll] returns a list of lists obtained by adding e to each list of [ll]. *)
let rec add_l e ll = match ll with
    | [] -> []
    | x::s -> (e::x)::add_l e s;;

(* [parts_l l] returns a list consisting of all subsets of elements of [l]. *)  
let rec parties_l l = match l with
    | [] -> [[]]
    | e::q -> let parties_q = parties_l q in
              parties_q @ (add_l e parties_q)

(* [array_of_list l] transform a list into a array *)
let array_of_list l = 
    let m = length_l l in
    let t = Array.make m 0 in
    let rec aux m1 l1 = match l1 with
        |[]->[||]
        |e::q -> t.(m-m1) <- e;
                if m1 = 1 then t
                else aux (m1 - 1) q in
aux m l;;

(* [array_to_list t] transform a array into a list *)
let array_to_list t = 
    let m = Array.length t in
    let rec aux i = 
        if i = 0 then []
        else t.(m-i)::aux (i-1) in
aux m;;
