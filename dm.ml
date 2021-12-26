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
