(*écrire ce que font les questions : liste doublement chainées cyclique*)
type 'a l2c = {elem : int; mutable prev : 'a l2c;mutable  next : 'a l2c}


let create_l2c e =
    let rec l = {elem = e; prev = l ; next = l } in l;;
    
      
let add_l2c l e =
    let l_new = {elem = e; prev = l; next = l.next } in
    l.next.prev <- l_new;
    l.next <- l_new;;
    
    
let suppr_l2c l =
    l.next.prev <- l.prev;
    l.prev.next <- l.next;;
    
    
let mem_l2c e l =
    let cur = ref l.next in
    while !cur.elem <> e && !cur != l do
        cur := !cur.next
    done;
    !cur.elem = e;;
    
    
let length_l2c l = 
    let rec aux l1 =
    if l1 == l then 1 else 1 + aux l1.next in aux l.next;;


let print_l2c l =
    let l_cur = ref l.next in 
    while !l_cur != l do 
        print_int !l_cur.elem; print_newline ();
        l_cur := !l_cur.next
        done;
        print_int !l_cur.elem;
        print_newline ();;
        
        
let fusion_l2c l1 l2 =
    (l1.next).prev <- l2;
    (l2.next).prev <- l1;
    let l1n = l1.next in
        l1.next<- l2.next;
        l2.next<- l1n;;
