type 'a file={tab: 'a array; mutable deb : int; mutable fin: int; mutable vide: bool};;

(* [ajout f e] add the element e in the pile*)
let ajoute f e =
  if f.deb = f.fin && not f.vide then failwith "full"
  else (f.tab.(f.fin) <-e;
       f.fin <- (f.fin + 1) mod Array.length f.tab;
       f.vide <- false);;

(* [retire f] delete the first element of the pile and returns this element *)
let retire f =
  if f.vide then failwith "empty"
  else let res = f.tab.(f.deb) in
        (f.deb <- (f.deb + 1) mod Array.length f.tab;
        f.vide <- f.deb = f.fin;
        res);;
        
