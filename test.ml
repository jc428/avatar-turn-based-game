open OUnit2
open Characters
(********************************************************************
  Re-using helper functions from A2 to compare set-like lists & print lists
 ********************************************************************)

(** [cmp_set_like_lists lst1 lst2] compares two lists to see whether
    they are equivalent set-like lists.  That means checking two things.
    First, they must both be {i set-like}, meaning that they do not
    contain any duplicates.  Second, they must contain the same elements,
    though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  &&
  List.length lst2 = List.length uniq2
  &&
  uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

  let get_names_test 
  (name: string)
  (ch : Characters.t)
  (expected_output : name list) : test = 
name >:: (fun _ -> 
    assert_equal expected_output (get_names ch)~cmp:cmp_set_like_lists 
    ~printer:(pp_list pp_string)) 

  let get_c_element_test 
  (name: string)
  (ch : Characters.t)
  (expected_output : element) : test = 
name >:: (fun _ -> 
    assert_equal expected_output (get_names ch)~cmp:cmp_set_like_lists 
    ~printer:(pp_list pp_string)) 

let ms1 = from_json "MS1satisfactory"

let characters_test =
[
  get_names_test "aang zuko charas" ms1 ["Aang"; "Zuko"];
]

let suite =
  "test suite for final proj"  >::: List.flatten [
    characters_test;
  ]

let _ = run_test_tt_main suite