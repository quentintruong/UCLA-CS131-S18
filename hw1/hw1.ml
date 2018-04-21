open List;;
open Pervasives;;

(* Duplicates + efficiency is stated as irrelevant
*)

(* Utility
*)
let rec print_list_int = function 
| [] -> ()
| e::l ->   print_int e ; 
                print_string "; " ; 
                print_list_int l;;

(* Symbol definition for grammar
*)
type ('nonterminal, 'terminal) symbol =
| N of 'nonterminal
| T of 'terminal;;

(*  1.
    Write a function subset a b that returns true iff a⊆b, i.e., if the set represented by the list a is a subset of the set represented by the list b. 
    Every set is a subset of itself. 
    This function should be generic to lists of any type: that is, the type of subset should be a generalization of 'a list -> 'a list -> bool.*)
let rec subset a b = match a, b with
| [], _ ->  true
| _, _ ->   if mem (hd a) b then
                subset (tl a) b
            else
                false;;

(*  2.
    Write a function equal_sets a b that returns true iff the represented sets are equal*)
let equal_sets a b =
    subset a b && subset b a;;

(*  3.
    Write a function set_union a b that returns a list representing a∪b*)
let set_union a b =
    let a2 = a @ b in
    sort_uniq compare a2;;

(*  4.
    Write a function set_intersection a b that returns a list representing a∩b. *)
let rec set_intersection a b = match a, b with
| [], _ ->  []
| _, _ ->   if mem (hd a) b then
                (hd a) :: set_intersection (tl a) b
            else
                set_intersection (tl a) b;;

(*  5.
    Write a function set_diff a b that returns a list representing a−b, 
    that is, the set of all members of a that are not also members of b. *)
let rec set_diff a b = match a, b with
| [], _ ->  []
| _, _ ->   if not (mem (hd a) b) then
                hd a :: set_diff (tl a) b
            else
                set_diff (tl a) b;;

(*  6.
    Write a function computed_fixed_point eq f x that returns the computed fixed point for f with respect to x, 
    assuming that eq is the equality predicate for f's domain. 
    A common case is that eq will be (=), that is, the builtin equality predicate of OCaml; 
    but any predicate can be used. 
    If there is no computed fixed point, your implementation can do whatever it wants: 
    for example, it can print a diagnostic, or go into a loop, or send nasty email messages to the user's relatives.*)
let rec computed_fixed_point eq f x = 
    if eq (f x) x then
        x
    else
        computed_fixed_point eq f (f x);;

(*  7.
    Write a function computed_periodic_point eq f p x 
    that returns the computed periodic point for f with period p and with respect to x, 
    assuming that eq is the equality predicate for f's domain.*)
let rec computed_periodic_point_helper f p x = match p with
| 0 -> x
| _ -> computed_periodic_point_helper f (p - 1) (f x);;

let rec computed_periodic_point eq f p x =
    if eq x (computed_periodic_point_helper f p x) then
        x
    else
        computed_periodic_point eq f p (f x);;

(*  8.
    Write a function while_away s p x that returns the longest list [x; s x; s (s x); ...] 
    such that p e is true for every element e in the list. 
    That is, if p x is false, return []; otherwise if p (s x) is false, 
    return [x]; otherwise if p (s (s x)) is false, return [x; s x]; and so forth. 
    For example, while_away ((+) 3) ((>) 10) 0 returns [0; 3; 6; 9]. 
    Your implementation can assume that p eventually returns false. *)
let rec while_away s p x = 
    if p x then
        x :: while_away s p (s x)
    else
        [];;

(*  9.
    Write a function rle_decode lp that decodes a list of pairs lp in run-length encoding form. 
    The first element of each pair is a nonnegative integer specifying the repetition length; 
    the second element is the value to repeat. 
    For example, rle_decode [2,0; 1,6] should return [0; 0; 6] and 
    rle_decode [3,"w"; 1,"x"; 0,"y"; 2,"z"] should return ["w"; "w"; "w"; "x"; "z"; "z"].*)
let rec pair_decode_helper count element = match count with
| 0 -> []
| _ -> element :: pair_decode_helper (count - 1) element;;

let pair_decode pair = match pair with 
| (count, element) -> pair_decode_helper count element;;

let rec rle_decode lp = match lp with
| [] -> []
| head::tail -> pair_decode head @ rle_decode tail;;

(*  10.
    Write a function filter_blind_alleys g that returns a copy of the grammar g 
    with all blind-alley rules removed. 
    This function should preserve the order of rules: 
    that is, all rules that are returned should be in the same order as the rules in g.*)
let check_if_terminal_symbol symbol terminal_symbols = match symbol with
| T symbol -> true
| N symbol -> mem symbol terminal_symbols;;

let rec check_if_terminal_rule rule terminal_symbols = match rule with
| [] -> true
| symbol::t -> (check_if_terminal_symbol symbol terminal_symbols) && (check_if_terminal_rule t terminal_symbols);;

(*  do one pass through all rules and append rules that are terminal and not already in terminal_rules *)
let rec extract_terminal_rules rules terminal_rules = match rules with
| [] -> terminal_rules
| rule::t -> if (not (mem rule terminal_rules)) && (check_if_terminal_rule (snd rule) (fst (split terminal_rules))) then 
                        extract_terminal_rules t (rule::terminal_rules)
                     else
                        extract_terminal_rules t terminal_rules;;

let filter_blind_alleys g =
    (* Extract all terminal rules by looping until fixed point; need multiple passes b/c gains knowledge of what is terminal as we go *)
    let unordered_terminal_rules = computed_fixed_point (=) (extract_terminal_rules (snd g)) [] in
    let ordered_terminal_rules = set_intersection (snd g) unordered_terminal_rules in
    (fst g, ordered_terminal_rules);;
