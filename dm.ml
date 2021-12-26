let mul_poly_naive t d =
    let degre = Array.length t - 1 in
    let resultat = Array.make((2*degre) + 1) 0. in
    for i = 0 to degre do
        for j = 0 to degre do
          resultat.(i + j) <- resultat.(i + j) +. t.(i) *. d.(j);
        done;
    done;
    resultat;;
    
    
    let t = [|1.;1.;2.|];;
    
    let d = [|2.;0.;1.|];
    
    
    type complexe = {re: float; im: float};; 
    
    let zero = {re = 0.; im = 0.};;
    
    let un = {re = 1.; im = 0.};;
    
    let conj z = {re = z.re ; im = -.(z.im)};;
    
    let add z1 z2 = {re = z1.re +. z2.re; im = z1.im +. z2.im};;
    
    let mul z1 z2 = {re = z1.re*.z2.re -. z1.im*.z2.im; im = z1.re*.z2.im +. z1.im*.z2.re};;


let rec horner l i = match l with 
    |[] -> zero;
    |e::q -> add e (mul i (horner q i));


let rec divise l = match l with 
    |[]->([],[])
    |[e] -> ([e],[])
    |e::e1::q -> let q1, q2 = divise q in 
                e::q1, e1::q2
                
                
                
                
let rec pow y x = 
    let x1 = x in
    let rec aux y' x' = 
    if x' > y' then false
    else let m = x1*x' in
        if m = y' then true
        else aux y' m in 
        aux y x;;
        
        
        
        
let rec ajout_end l e = match l with
    |[]->[e]
    |e1::q -> e1::(puiss3 q e);;
    
    
let rec puiss2 l = 
    let t = List.length l in
    if pow t 2 
    then l 
    else puiss2 (ajout_end l zero);;
        


let completer l = 
    let t = List.length l in 
    let rec aux l' =
        let t' = List.length l' in
        if t' = 2*t - 1
            then l'
        else aux (ajout_end l' zero) in
        aux l;;


let rec mul_ft l1 l2 = match l1,l2 with
    |[],l2->l2
    |l1,[]->l1
    |e1::q1,e2::q2 -> (mul e1 e2)::mul_ft q1 q2;;


let coeff l =
    let n = {re = 1./.float_of_int(List.length l);im = 1./.float_of_int(List.length l)} in
    let rec aux l' = match l' with
        |[]->[]
        |e::q -> (mul n e)::aux q in
        aux l
