open OUnit2
open Characters
open Battle
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

(** start of Characters test *)
let get_names_test 
    (name: string)
    (ch : Characters.t)
    (expected_output : name list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_names ch)~cmp:cmp_set_like_lists 
        ~printer:(pp_list pp_string)) 

let get_c_element_test 
    (test_name: string)
    (character_name: name)
    (ch : Characters.t)
    (expected_output : element) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_c_element ch character_name))

let get_stats_test 
    (test_name: string)
    (character_name: name)
    (ch : Characters.t)
    (expected_output : stats) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_stats ch character_name))

let get_moves_test 
    (test_name: string)
    (character_name: name)
    (ch : Characters.t)
    (expected_output : move list) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_moves ch character_name))

let get_c_description_test 
    (test_name: string)
    (character_name: name)
    (ch : Characters.t)
    (expected_output : string) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_c_description ch character_name))

let get_move_by_id_test 
    (test_name: string)
    (character_name: name)
    (ch : Characters.t)
    (id: int)
    (expected_output : move) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_move_by_id ch character_name id))

let ms1 = from_json "MS1satisfactory"

let characters_tests =
  [
    get_names_test "aang zuko charas" ms1 ["Aang"; "Zuko"];
    get_c_element_test "aang element" "Aang" ms1 Avatar;
    get_c_element_test "zuko element" "Zuko" ms1 Fire;
    get_stats_test "aang stats" "Aang" ms1 {
      health = 100.0;
      power =  1.0;
      speed = 1.0;
      evasiveness = 1.0;
    };
    get_stats_test "zuko stats" "Zuko" ms1 {
      health = 69.0;
      power =  1.0;
      speed = 1.0;
      evasiveness = 1.0;
    };
    get_moves_test "aang moves" "Aang" ms1 [{
        id = 1;
        is_super =  false;
        m_name = "Air blast";
        m_element = Air;
        m_description = "Aang shoots a powerful blast of air from his staff";
        damage = 15.;
        pp = 10;
      }];

    get_moves_test "zuko moves" "Zuko" ms1 [{
        id = 1;
        is_super =  false;
        m_name = "Fire ball";
        m_element = Fire;
        m_description = "Zuko shoots a powerful blast of fire";
        damage = 15.0;
        pp = 10;
      }];
    get_c_description_test "zuko desc" "Zuko" ms1 "DISGRACED PRINCE RAWRRR XD";
    get_c_description_test "aang desc" "Aang" ms1 "hes da   Avatar broo0o";
  ]

(* start of battle tests*)
let extract legal = 
  match legal with 
  | Legal t -> t
  | IllegalInvalidMove -> raise (Failure "IllegalInvalidMove")
  | IllegalNoPP -> raise (Failure "IllegalNoPP")


let ba = init_battle ms1 
let ba2_raw = make_move ba "Aang" 1
let ba2 = extract ba2_raw

let get_current_health_test 
    (name : string) 
    (ba : battle)
    (name : name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_current_health ba name)
        ~printer: string_of_float)

let get_current_pp_test 
    (name : string) 
    (ba : battle)
    (name : name)
    (move_id : int)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_current_pp ba name move_id)
        ~printer: string_of_int)

let set_new_health_test
    (name : string) 
    (ba : battle)
    (user_name : name)
    (user_move_id : int)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (set_new_health ba user_name user_move_id)
        ~printer: string_of_float) 

let set_new_pp_test
    (name : string) 
    (ba : battle)
    (name : name)
    (move_id : int)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (set_new_pp ba name move_id)
        ~printer: string_of_int)

let battle_tests =
  [
    get_current_health_test "initial health Aang" ba "Aang" 100.0;
    get_current_health_test "initial health Zuko" ba "Zuko" 69.0;

    get_current_pp_test "initial pp of Aang move 1" ba "Aang" 1 10;
    get_current_pp_test "initial pp of Zuko move 1" ba "Zuko" 1 10;

    set_new_health_test "Aang attacks Zuko w/ move 1" ba "Aang" 1 54.0;
    set_new_health_test "Zuko attacks Aang w/ move 1" ba "Zuko" 1 85.0;

    set_new_pp_test "Aang uses move 1" ba "Aang" 1 9;
    set_new_pp_test "Zuko uses move 1" ba "Zuko" 1 9;

    get_current_health_test "post-move health Aang" ba2 "Aang" 100.0;
    get_current_health_test "post-move health Zuko" ba2 "Zuko" 54.0;
    get_current_pp_test "post-move pp of Aang move 1" ba2 "Aang" 1 9;
  ]

let suite =
  "test suite for final proj"  >::: List.flatten [
    characters_tests;
    battle_tests;
  ]

let _ = run_test_tt_main suite