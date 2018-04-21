open List;;
open Pervasives;;

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

type awksub_nonterminals =
    | Expr | Lvalue | Incrop | Binop | Num

(*
Traverses the rules
    If current nonterminal is equivalent to prev, adds rules to curr_alt_list
    If current nonterminal is not equivalent to prev, adds the pair (nonterminal, alternative_list) to the production pair list
Returns the production pair list
*)
let rec create_production_pair_list nonterminals rhss prev curr_alt_list production_pair_list = match nonterminals, rhss with
    | [] , [] -> production_pair_list @ [(prev, curr_alt_list)]
    | nt :: nt_t, rhs :: rhs_t when nt = prev -> create_production_pair_list nt_t rhs_t prev (curr_alt_list @ [rhs]) production_pair_list
    | nt :: nt_t, rhs :: rhs_t -> create_production_pair_list nonterminals rhss nt [] (production_pair_list @ [(prev, curr_alt_list)])
    | _, _ -> [];;

let create_production_function rules = 
    let (nonterminals, rhss) = split rules in
    let production_pair_list = create_production_pair_list nonterminals rhss (hd nonterminals) [] [] in
    fun key -> List.assoc key production_pair_list;;

let convert_grammar gram1 = match gram1 with 
    | (start_symbol, rules) ->  
        let production_function = create_production_function rules in
        (start_symbol, production_function);;

(*
            Matcher for Terminal "3"
           +----------------------------------+
acceptor   |                                  |
+----------------------------------------+    |
           |                             |    |
           |    +-----------------+   +--v--+ |
           |    | PatternMatching | d |Eval | |
frags      |    | for             +--->     | |
+---------------> Terminal "3"    | s |     +----> Some (d,s)
["3";"+";  |    |                 +--->     | |    d = [(Num, T "3")]
  "4"]     |    +-----------------+   +-----+ |    s = ["+"; "4"]
           |                                  |
           +----------------------------------+   or None (if rejected)

             +------------------------------------------------------------------------+
             |                                                                        |
+--------------------------------------------------------------------------+          |
Acceptor     |                            Internal Acceptor*               |          |
(Top-level)  | +----------------------+   +---------------------------------------+   |
             | |                      |   |                                |      |   |
             | | Pattern Matching     |d1 | +-----------------------+ +----v----+ |   |
             | | for Terminal "3"     +---> |                     d2+->         | |   |
             | |                      |   | | Pattern Matching      | | Eval    | |   |
             | |                      |   | | for Terminal "+"      | |         +-------->Some(d2,s2)
             | |                      |s1 | |                     s2+->         | |   |   or None
+-------------->                      +---> +-----------------------+ +---------+ |   |
Fragments    | |                      |   |                                       |   |
["3";"+";"4"]| +----------------------+   +---------------------------------------+   |
             +------------------------------------------------------------------------+
- Diagrams from TA

    rhs_matcher traverses alternative_list to find a valid rhs
    symbol_matcher traverses rule to find valid terminal symbols
    rhs_matcher and symbol_matcher are mutually recursive
    acceptor function is dynamically overriden
    matcher initiates calling production function on the start symbol 
        and passing the resulting alternative list to rhs_matcher
    parse_prefix returns a matcher specific to the grammar
        by giving it the start_symbol and production_function
*)
let rec rhs_matcher start prod_func alternative_list acceptor derivation fragment = match alternative_list with 
    | [] -> (* if empty alternative list, then fail *)
        None 
    | rule :: other_rules -> (* try the symbols in the rhs *)
        let some_x = symbol_matcher rule prod_func acceptor (derivation @ [(start, rule)]) fragment in 
        match some_x with
            | Some x -> (* completed *)
                Some x 
            | None -> (* failed to complete, try next rhs *)
                rhs_matcher start prod_func other_rules acceptor derivation fragment 
and symbol_matcher rule prod_func acceptor derivation fragment = match rule with 
    | [] -> (* this acceptor function is dynamically overridden; may follow other symbols *)
        acceptor derivation fragment 
    | N sym :: other_syms -> (* found nonterminal *)
        let modified_matcher = symbol_matcher other_syms prod_func acceptor in (* wrap acceptor so that it will continue matching other symbols *)
        let alternative_list = prod_func sym in (* follow nonterminal symbol *)
        rhs_matcher sym prod_func alternative_list modified_matcher derivation fragment (* follow nonterminal symbol *)
    | T sym :: other_syms -> (* found terminal *)
        match fragment with
            | terminal :: other_terminals when terminal = sym -> (* found correct terminal, move onto next symbols *)
                symbol_matcher other_syms prod_func acceptor derivation other_terminals 
            | _ -> (* found incorrect terminal *)
                None;; 

let matcher start_sym prod_func acceptor fragment = 
    let alternative_list = prod_func start_sym in
    rhs_matcher start_sym prod_func alternative_list acceptor [] fragment;; (* start by searching start_sym's alternative list *)

let parse_prefix gram = match gram with
    | start_sym, prod_func ->
        matcher start_sym prod_func;;
