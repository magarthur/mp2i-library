(*stack with list*)

(*[stack_empty_l p returns if p is empty *)
let stack_empty_l p = p = [];;

(* [stack_push_l p e] push e in the stack p *)
let stack_push_l p e =  e::p;;

(* [stack_pop_l p] retires the first element and give this element *)
let stack_pop_l p = match p with
  |[] -> failwith "stack is empty"
  |e::q -> (e,q);;
  
  
(* stack with array *)
type 'a stack = { t: 'a array ; mutable n : int } (* n is the number of elements in the stack *)

(* [stack_empty_t p] returns if p is empty*)
let stack_empty_t p = p.n = 0;;

(* [stack_push_t p e] push e in the stack p *)
let stack_push_t p e =
  if p.n = Array.length p.t then failwith "full"
  else (p.t.(p.n) <- e;
        p.n <- p.n +1);;
        
(* [stack_pop_t p] retires the first element of the stack and give this element *)
let stack_pop_t p e =
  if p.n = 0 then failwith "stack is empty"
  else (p.n <- p.n -1 ;p.t.(p.n));;
