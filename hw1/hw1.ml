open List;;
open Pervasives;;


(* call other recursive function from function
let rec f2 x = match x with
    | 0 -> "a"
    | _ -> "a" ^ f2 (x - 1);;

let f1 x =
    f2 x;;

let out = f1 5;;
print_string out;;
*)


(*
let rec subset a b = match a with
| 1 -> "a"
| 2 -> "b";;

let subset1 = subset 1;;
let v1 = subset1 1;;
print_string v1;;
*)

(* 
UTILITY
*)
let rec print_list = function 
    [] -> ()
    | e::l ->   print_int e ; 
                print_string "; " ; 
                print_list l;;


(* 
1.
Write a function subset a b that returns true iff a⊆b, i.e., if the set represented by the list a is a subset of the set represented by the list b. 
Every set is a subset of itself. 
This function should be generic to lists of any type: that is, the type of subset should be a generalization of 'a list -> 'a list -> bool.
*)
let rec subset_helper l1 l2 = match l1, l2 with
| [], _ ->  true
| _, [] ->  false
| _, _ ->   if hd l1 = hd l2 then    
                subset_helper (tl l1) (tl l2)
            else
                subset_helper l1 (tl l2);;

let subset l1 l2 = 
    subset_helper (sort_uniq compare l1) (sort_uniq compare l2);;

(*
let out = string_of_bool (subset l1 l2);;
print_string out;;
print_endline "";;
*)


(*
2.
Write a function equal_sets a b that returns true iff the represented sets are equal
*)
let equal_sets l1 l2 =
    subset l1 l2 && subset l2 l1;;

(*
let out = string_of_bool (equal_sets l1 l2);;
print_string out;;
print_endline "";;
*)


(*
3.
Write a function set_union a b that returns a list representing a∪b.
*)
let set_union l1 l2 =
    let l12 = l1 @ l2 in
    sort_uniq compare l12;;

(*
let out = set_union l1 l2;;
print_list out;;
print_endline "";;
*)


(*
4.
Write a function set_intersection a b that returns a list representing a∩b.
*)
let rec set_intersection_helper l1 l2 = match l1, l2 with 
| [], _ ->  []
| _, [] ->  []
| _, _ ->   if hd l1 > hd l2 then
                set_intersection_helper l1 (tl l2)
            else if hd l1 < hd l2 then
                set_intersection_helper (tl l1) l2
            else
                hd l1 :: set_intersection_helper (tl l1) (tl l2);;

let set_intersection l1 l2 =
    set_intersection_helper (sort_uniq compare l1) (sort_uniq compare l2);;

(*
let out = set_intersection l1 l2;;
print_list out;;
print_endline "";;
*)



let l1 = [1;2;3;4];;
let l2 = [-1;4];;


(*
5
Write a function set_diff a b that returns a list representing a−b, 
that is, the set of all members of a that are not also members of b.
*)

let rec set_diff_helper l1 l2 = match l1, l2 with
| [], _ ->  []
| _, [] ->  l1
| _, _ ->   if hd l1 > hd l2 then
                set_diff_helper l1 (tl l2)
            else if hd l1 < hd l2 then
                hd l1 :: set_diff_helper (tl l1) l2
            else 
                set_diff_helper (tl l1) (tl l2)

let set_diff l1 l2 = 
    set_diff_helper (sort_uniq compare l1) (sort_uniq compare l2);;

(*
let out = set_diff l1 l2;;
print_list out;;
print_endline "":;
*)

(*
6
Write a function computed_fixed_point eq f x 
that returns the computed fixed point for f with respect to x, 
assuming that eq is the equality predicate for f's domain. 
A common case is that eq will be (=), 
that is, the builtin equality predicate of OCaml; 
but any predicate can be used. 
If there is no computed fixed point, 
your implementation can do whatever it wants: 
for example, it can print a diagnostic, 
or go into a loop, 
or send nasty email messages to the user's relatives.
*)

let rec computed_fixed_point eq f x = 
    if eq (f x) x then
        x
    else
        computed_fixed_point eq f (f x);;

(*
let out = computed_fixed_point (=) (fun x -> x / 2) 1000000000;;
print_int out;;
*)


(*
7
Write a function computed_periodic_point eq f p x 
that returns the computed periodic point for f with period p and with respect to x, 
assuming that eq is the equality predicate for f's domain.
*)

let rec computed_periodic_point_helper f p x = match p with
| 0 -> x
| _ -> computed_periodic_point_helper f (p - 1) (f x);;

let rec computed_periodic_point eq f p x =
    if eq x (computed_periodic_point_helper f p x) then
        x
    else
        computed_periodic_point eq f p (f x);;
    
(*
let out = computed_periodic_point (=) (fun x -> x / 2) 0 (-1);;
print_int out;;

let out2 = computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.5;;
print_float out2;;
*)


(*
Write a function while_away s p x that returns the longest list [x; s x; s (s x); ...] 
such that p e is true for every element e in the list. 
That is, if p x is false, return []; otherwise if p (s x) is false, 
return [x]; otherwise if p (s (s x)) is false, return [x; s x]; and so forth. 
For example, while_away ((+) 3) ((>) 10) 0 returns [0; 3; 6; 9]. 
Your implementation can assume that p eventually returns false.
*)

let while_away s p x = 



