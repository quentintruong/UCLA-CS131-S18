let accept_all derivation string = Some (derivation, string)
let accept_empty_suffix derivation = function
    | [] -> Some (derivation, [])
    | _ -> None

type qt_nonterminals =
    | First | Second | Third | Fourth | Fifth | A | B | ABA 

let qt_grammar =
    (First,
    function
    | First ->
        [[N Second]]
    | Second ->
        [[N Third]]
    | Third -> 
        [[N Fourth]]
    | Fourth ->
        [[N Fifth]]
    | Fifth ->
        [[T "3"; N ABA];
        [T "3"]]
    | A ->
        [[T "A"]]
    | B -> 
        [[T "B"]]
    | ABA -> 
        [[N A; N B; N A]]);;

let test_1 = (parse_prefix qt_grammar accept_all ["3"; "3"]) = Some
   ([(First, [N Second]); (Second, [N Third]); (Third, [N Fourth]);
     (Fourth, [N Fifth]); (Fifth, [T "3"])],
    ["3"]);;

let test_2 = (parse_prefix qt_grammar accept_all ["3"; "A"; "B"; "A"]) = Some
   ([(First, [N Second]); (Second, [N Third]); (Third, [N Fourth]);
     (Fourth, [N Fifth]); (Fifth, [T "3"; N ABA]); (ABA, [N A; N B; N A]);
     (A, [T "A"]); (B, [T "B"]); (A, [T "A"])],
    []);;
