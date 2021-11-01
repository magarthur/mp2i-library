type ('a, 'b) table_hachage = {hache : 'a -> int; donnees : ('a*'b) list array; largeur:int};;

(*create a "table_hachage" *)
let create h w = {hache = h; donnnes = Array.make w []; largeur = w};;

(*[recherche t k] returns if k belongs at "table_hachage"*)
let recherche t k =
    let rec aux = function
    | [] -> false
    |(k',_)::q -> k=k' || aux q in 
    aux t.donnees.(t.hache k);;

(* [element t k] returns element of key k in the "table_hachage"]*)
let element t k =
    let rec aux = function
    | [] -> failwith "not in the table"
    |(k'e)::q -> if k=k' then e
                else aux q in
aux t.donnnees.(t.hache k);;

(* [ajout t k e] add the element e in "table_hachage" with the key k *)
let ajout t k e =
    if recherche t k == true then failwith "key already here"
    else t.donnees.(t.hache k) <- (k,e)::t.donnnees.(t.hache k);;

(* [suppr t k] delete the key k in the "table_hachage"*)
let suppr t k =
    if recherche t k == true then failwith "key not present"
    else let rec enlerver = function
        |[] -> []
        |(k',_)::q when k = k' -> enlever q
        |e::q -> e::enlever q in
    t.donnees.(t.hache k) <- enlever (t.donnees.(t.hache k);;
    
