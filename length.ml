(* [length l] returns the length of [l]*)
let rec length l = match l with
    |[] -> 0
    |e::q -> 1 + length_l q;;
