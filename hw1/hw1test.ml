print_endline ("Testing subset");;

let my_subset_test0 = subset [] [] = true;;
print_endline (string_of_bool my_subset_test0);;

let my_subset_test1 = subset [] [1] = true;;
print_endline (string_of_bool my_subset_test1);;

let my_subset_test2 = subset [1] [1] = true;;
print_endline (string_of_bool my_subset_test2);;

let my_subset_test3 = subset [1] [] = false;;
print_endline (string_of_bool my_subset_test3);;

let my_subset_test4 = subset [1;2;3;4] [1;2;3] = false;;
print_endline (string_of_bool my_subset_test4);;

let my_subset_test5 = subset [3;2;1] [1;2;3] = true;;
print_endline (string_of_bool my_subset_test5);;

let my_subset_test6 = subset [4;3;2;1] [1;2;3] = false;;
print_endline (string_of_bool my_subset_test6);;

let my_subset_test7 = subset [1;2;3] [4;5;6] = false;;
print_endline (string_of_bool my_subset_test7);;

let my_subset_test8 = subset ["a"] ["a"] = true;;
print_endline (string_of_bool my_subset_test8);;

let my_subset_test9 = subset ["a"] ["b"] = false;;
print_endline (string_of_bool my_subset_test9);;

let my_subset_test10 = subset ['a'] ['a'] = true;;
print_endline (string_of_bool my_subset_test10);;

let my_subset_test11 = subset ['a'] ['b'] = false;;
print_endline (string_of_bool my_subset_test11);;



print_endline "Testing equal_sets"

let my_equal_sets_test0 = equal_sets [] [] = true;;
print_endline (string_of_bool my_equal_sets_test0);;

let my_equal_sets_test1 = equal_sets [] [1] = false;;
print_endline (string_of_bool my_equal_sets_test1);;

let my_equal_sets_test2 = equal_sets [1] [] = false;;
print_endline (string_of_bool my_equal_sets_test2);;

let my_equal_sets_test3 = equal_sets [1] [1] = true;;
print_endline (string_of_bool my_equal_sets_test3);;

let my_equal_sets_test4 = equal_sets [1] [1;2] = false;;
print_endline (string_of_bool my_equal_sets_test4);;

let my_equal_sets_test5 = equal_sets [1;2] [1] = false;;
print_endline (string_of_bool my_equal_sets_test5);;

let my_equal_sets_test6 = equal_sets [1;2] [1;2] = true;;
print_endline (string_of_bool my_equal_sets_test6);;

let my_equal_sets_test7 = equal_sets [2;1] [1;2] = true;;
print_endline (string_of_bool my_equal_sets_test7);;

let my_equal_sets_test8 = equal_sets [1;2] [3;4] = false;;
print_endline (string_of_bool my_equal_sets_test8);;

let my_equal_sets_test9 = equal_sets ["a"] ["a"] = true;;
print_endline (string_of_bool my_equal_sets_test9);;

let my_equal_sets_test10 = equal_sets ["a"] ["b"] = false;;
print_endline (string_of_bool my_equal_sets_test10);;

let my_equal_sets_test11 = equal_sets ['a'] ['a'] = true;;
print_endline (string_of_bool my_equal_sets_test11);;

let my_equal_sets_test12 = equal_sets ['a'] ['b'] = false;;
print_endline (string_of_bool my_equal_sets_test12);;



print_endline "Testing set_union"

let my_set_union_test0 = set_union [] [] = [];;
print_endline (string_of_bool my_set_union_test0);;

let my_set_union_test1 = set_union [] [1] = [1];;
print_endline (string_of_bool my_set_union_test1);;

let my_set_union_test2 = set_union [1] [] = [1];;
print_endline (string_of_bool my_set_union_test2);;

let my_set_union_test3 = set_union [1] [2] = [1;2];;
print_endline (string_of_bool my_set_union_test3);;

let my_set_union_test4 = set_union ["a"] ["b"] = ["a";"b"];;
print_endline (string_of_bool my_set_union_test4);;

let my_set_union_test5 = set_union ['a'] ['b'] = ['a';'b'];;
print_endline (string_of_bool my_set_union_test5);;



print_endline "Testing set_intersection";;

let my_set_intersection_test0 = set_intersection [] [] = [];;
print_endline (string_of_bool my_set_intersection_test0);;

let my_set_intersection_test1 = set_intersection [1] [] = [];;
print_endline (string_of_bool my_set_intersection_test1);;

let my_set_intersection_test2 = set_intersection [] [1] = [];;
print_endline (string_of_bool my_set_intersection_test2);;

let my_set_intersection_test3 = set_intersection [1] [1] = [1];;
print_endline (string_of_bool my_set_intersection_test3);;

let my_set_intersection_test4 = set_intersection [1;2;3] [1;2;3] = [1;2;3];;
print_endline (string_of_bool my_set_intersection_test4);;

let my_set_intersection_test5 = set_intersection [1] [1;2;3] = [1];;
print_endline (string_of_bool my_set_intersection_test5);;

let my_set_intersection_test6 = set_intersection [1;2] [2;3] = [2];;
print_endline (string_of_bool my_set_intersection_test6);;

let my_set_intersection_test7 = set_intersection [1;2] [3;4] = [];;
print_endline (string_of_bool my_set_intersection_test7);;

let my_set_intersection_test8 = set_intersection [1;2;3] [3;2;1] = [1;2;3];;
print_endline (string_of_bool my_set_intersection_test8);;

let my_set_intersection_test9 = set_intersection ["a"] ["a";"b"] = ["a"];;
print_endline (string_of_bool my_set_intersection_test9);;

let my_set_intersection_test10 = set_intersection ["a"] ["b"] = [];;
print_endline (string_of_bool my_set_intersection_test10);;

let my_set_intersection_test11 = set_intersection ['a'] ['a';'b'] = ['a'];;
print_endline (string_of_bool my_set_intersection_test11);;

let my_set_intersection_test12 = set_intersection ['a'] ['b'] = [];;
print_endline (string_of_bool my_set_intersection_test12);;



print_endline "Testing set_diff";;

let my_set_diff_test0 = set_diff [] [] = [];;
print_endline (string_of_bool my_set_diff_test0);;

let my_set_diff_test1 = set_diff [0] [0] = [];;
print_endline (string_of_bool my_set_diff_test1);;

let my_set_diff_test2 = set_diff [0] [1] = [0];;
print_endline (string_of_bool my_set_diff_test2);;

let my_set_diff_test3 = set_diff [0] [] = [0];;
print_endline (string_of_bool my_set_diff_test3);;

let my_set_diff_test4 = set_diff [0;1;2] [0;1] = [2];;
print_endline (string_of_bool my_set_diff_test4);;

let my_set_diff_test5 = set_diff [0;1] [0;1;2] = [];;
print_endline (string_of_bool my_set_diff_test5);;

let my_set_diff_test6 = set_diff [2;1;0] [0;1;2] = [];;
print_endline (string_of_bool my_set_diff_test6);;

let my_set_diff_test7 = set_diff [5;4;3;2;1] [0;1;2] = [5;4;3];;
print_endline (string_of_bool my_set_diff_test7);;

let my_set_diff_test8 = set_diff ["a"] ["a"] = [];;
print_endline (string_of_bool my_set_diff_test8);;

let my_set_diff_test9 = set_diff ["a"] ["b"] = ["a"];;
print_endline (string_of_bool my_set_diff_test9);;

let my_set_diff_test10 = set_diff ['a'] ['a'] = [];;
print_endline (string_of_bool my_set_diff_test10);;

let my_set_diff_test11 = set_diff ['a'] ['b'] = ['a'];;
print_endline (string_of_bool my_set_diff_test11);;



print_endline "Testing computed_fixed_point";;

let my_computed_fixed_point_test0 = computed_fixed_point (=) (fun x -> x / 5) 5 = 0;;
print_endline (string_of_bool my_computed_fixed_point_test0);;



print_endline "Testing computed_periodic_point"

let my_computed_periodic_point_test0 = computed_periodic_point (=) (fun x -> x *. x -. 1.) 2 0.0 = 0.;;
print_endline (string_of_bool my_computed_periodic_point_test0);;



print_endline "Testing while_away";;

let my_while_away_test0 = while_away ((+) 3) ((>) 13) (-6) = [-6;-3;0;3;6;9;12];;
print_endline (string_of_bool my_while_away_test0);;



print_endline "Testing rle_decode";;

let my_rle_decode_test0 = rle_decode [3,0; 2,-1; 0,5; 1,6] = [0;0;0; -1;-1; 6];;
print_endline (string_of_bool my_rle_decode_test0);;



print_endline "Testing rle_decode";;

type giant_nonterminals =
  | Conversation | Sentence | Grunt | Snore | Shout | Quiet;;

let giant_grammar =
  Conversation,
  [Snore, [T"ZZZ"];
   Quiet, [];
   Grunt, [T"khrgh"];
   Shout, [T"aooogah!"];
   Sentence, [N Quiet];
   Sentence, [N Grunt];
   Sentence, [N Shout];
   Conversation, [N Snore];
   Conversation, [N Sentence; T","; N Conversation]];;

let my_filter_blind_alleys_test0 = filter_blind_alleys (Conversation, [hd (snd giant_grammar)]) 
                                    = (Conversation, [(Snore, [T "ZZZ"])]);;

print_endline (string_of_bool my_filter_blind_alleys_test0);;
