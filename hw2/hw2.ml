open List;;
open Pervasives;;

type ('nonterminal, 'terminal) symbol =
    | N of 'nonterminal
    | T of 'terminal

type awksub_nonterminals =
    | Expr | Lvalue | Incrop | Binop | Num

let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]

let awksub_grammar = (Expr, awksub_rules);;

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

(* 
Splits rules and turns the production pair list into a map
*)
let create_production_functions rules = 
    let (nonterminals, rhss) = split rules in
    let production_pair_list = create_production_pair_list nonterminals rhss (hd nonterminals) [] [] in
    fun key -> List.assoc key production_pair_list;;

let convert_grammar gram1 = match gram1 with 
| (start_symbol, rules) ->  let production_functions = create_production_functions rules in
                            (start_symbol, production_functions);;

let gram2 = convert_grammar awksub_grammar;;
let func = snd gram2;;


