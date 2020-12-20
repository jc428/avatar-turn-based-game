open OUnit2
open Characters
open Battle
open Mp_battle
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

let get_new_moves_test
    (test_name : string)
    (ch : Characters.t)
    (expected_output : move list ) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (get_new_moves ch))


let ms1 = from_json "MS1multiplemoves"

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
        damage = 15.0;
        pp = 10;
      };
       {
         id = 2;
         is_super =  false;
         m_name = "Tornado whirl";
         m_element = Air;
         m_description = "Aang spins a dazzling whirlwind at his opponent";
         damage = 10.0;
         pp = 20;
       }];

    get_moves_test "zuko moves" "Zuko" ms1 [{
        id = 1;
        is_super =  false;
        m_name = "Fire ball";
        m_element = Fire;
        m_description = "Zuko shoots a powerful blast of fire";
        damage = 15.0;
        pp = 10;
      };
       {
         id = 2;
         is_super =  false;
         m_name = "Fire breath";
         m_element = Fire;
         m_description = "Zuko harnesses his inner dragon, unleashing a breath of fire";
         damage = 8.0;
         pp = 25;
       };
      ];

    get_new_moves_test "Aang new moves" ms1 [{
        id = 5;
        is_super =  false;
        m_name = "Fire breath";
        m_element = Fire;
        m_description = "Aang harnesses his inner dragon, unleashing a breath of fire";
        damage = 8.0;
        pp = 25;
      }];
    get_c_description_test "zuko desc" "Zuko" ms1 "DISGRACED PRINCE RAWRRR XD";
    get_c_description_test "aang desc" "Aang" ms1 "hes da   Avatar broo0o";
  ]

(* start of battle tests*)
let extract (legal : Battle.result) = 
  match legal with 
  | Legal t -> t
  | IllegalInvalidMove -> raise (Failure "IllegalInvalidMove")
  | IllegalNoPP -> raise (Failure "IllegalNoPP")
  | IllegalStat -> raise (Failure "IllegalStat")


let ba = init_battle ms1 
let ba2_raw = make_move ba "Aang" 1
let ba2 = extract ba2_raw
let ba3_raw = make_move ba "Aang" 1
let ba3 = extract ba3_raw
let ba4_raw = make_move ba "Zuko" 1
let ba4 = extract ba4_raw

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

let update_moves_test
    (test_name : string)
    (battle : battle)
    (name : Characters.name)
    (old_move_id : int)
    (new_move_id : int)
    (expected_output : Characters.move list) : test =
  test_name >:: (fun _ -> assert_equal expected_output
                    (update_moves battle name old_move_id new_move_id))

let update_stats_test
    (test_name : string)
    (battle : battle)
    (name : Characters.name)
    (stat : string)
    (mult : float)
    (s: t2 option)
    (expected_output : Characters.stats) : test =
  test_name >:: (fun _ -> assert_equal expected_output
                    (update_stats battle name stat mult s))

let get_enemy_moves_test
    (name : string)
    (ba : battle)
    (expected_output : Characters.move list) : test =
  name >:: (fun _ -> assert_equal expected_output (get_enemy_moves ba))

let battle_tests =
  [
    get_current_health_test "initial health Aang" ba "Aang" 100.0;
    get_current_health_test "initial health Zuko" ba "Zuko" 69.0;
    "name does not exist" >:: 
    (fun _ -> assert_raises 
        (Failure "name does not belong to player or enemy") 
        (fun () -> get_current_health ba "Borat"));

    get_current_pp_test "initial pp of Aang move 1" ba "Aang" 1 10;
    "name does not exist" >:: 
    (fun _ -> assert_raises 
        (Failure "name does not belong to player or enemy") 
        (fun () -> get_current_pp ba "Borat" 1));

    set_new_health_test "Aang attacks Zuko w/ move 1" ba "Aang" 1 54.0;
    set_new_health_test "Zuko attacks Aang w/ move 1" ba "Zuko" 1 85.0;
    "name does not exist" >:: 
    (fun _ -> assert_raises 
        (Failure "name does not belong to player or enemy") 
        (fun () -> set_new_health ba "Borat" 1));

    set_new_pp_test "Aang uses move 1" ba "Aang" 1 9;
    "name does not exist" >:: 
    (fun _ -> assert_raises 
        (Failure "name does not belong to player or enemy") 
        (fun () -> set_new_pp ba "Borat" 1));

    get_current_health_test "post-move health Aang" ba2 "Aang" 100.0;
    get_current_health_test "post-move health Zuko" ba2 "Zuko" 54.0;
    get_current_pp_test "post-move pp of Aang move 1" ba2 "Aang" 1 9;
    "aang move 69 should not exist" >:: (fun _ -> 
        assert_equal
          (make_move ba3 "Aang" 69) 
          (IllegalInvalidMove));
    (* "aang can't do move 1 cuz no pp" >:: (fun _ -> 
        assert_equal
          (make_move ba3 "Aang" 1) 
          (IllegalNoPP)); *)

    (* update_moves_test "swap move 1 with 5" ba "Aang" 1 5  *)

    (* update_stats_test "multiply Aang health by 20%" ba "Aang" "health" 1.2  *)

    get_enemy_moves_test "initial e moves" ba [{
        id = 1;
        is_super =  false;
        m_name = "Fire ball";
        m_element = Fire;
        m_description = "Zuko shoots a powerful blast of fire";
        damage = 15.0;
        pp = 10;
      };
       {
         id = 2;
         is_super =  false;
         m_name = "Fire breath";
         m_element = Fire;
         m_description = "Zuko harnesses his inner dragon, unleashing a breath of fire";
         damage = 8.0;
         pp = 25;
       }];
    get_enemy_moves_test "e moves after 1 used" ba4 [{
        id = 1;
        is_super =  false;
        m_name = "Fire ball";
        m_element = Fire;
        m_description = "Zuko shoots a powerful blast of fire";
        damage = 15.0;
        pp = 9;
      };
       {
         id = 2;
         is_super =  false;
         m_name = "Fire breath";
         m_element = Fire;
         m_description = "Zuko harnesses his inner dragon, unleashing a breath of fire";
         damage = 8.0;
         pp = 25;
       }]
  ]

(* multiplayer battle test *)
let mp_current_health_test 
    (name : string) 
    (btl : Mp_battle.t)
    (name : name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (current_health btl name) 
        ~printer: string_of_float)

let mp_current_pp_test 
    (name : string) 
    (btl : Mp_battle.t)
    (name : name)
    (move_id : int)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (current_pp btl name move_id)
        ~printer: string_of_int)

let mp_new_health_test
    (name : string) 
    (btl : Mp_battle.t)
    (name : name)
    (move_id : int)
    (target : name)
    (expected_output : float) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (new_health btl name move_id target)
        ~printer: string_of_float) 

let mp_extract (legal : Mp_battle.result) = 
  match legal with 
  | Legal t -> t
  | IllegalInvalidMove -> raise (Failure "IllegalInvalidMove")
  | IllegalNoPP -> raise (Failure "IllegalNoPP")

let btl = mp_init_battle ["Aang";"Zuko";"Ty Lee";"Katara"] 
let btl2_raw = mp_make_move btl "Aang" 1 "Ty Lee"
let btl2 = mp_extract btl2_raw

(* let ba3_raw = mp_make_move ba "Aang" 1
   let ba3 = extract ba3_raw
   let ba4_raw = make_move ba "Zuko" 1
   let ba4 = extract ba4_raw *)

let mp_battle_tests = [
  mp_current_health_test "mp initial health Aang" btl "Aang" 100.0;
  mp_current_health_test "mp initial health Zuko" btl "Zuko" 100.0;
  "name does not exist" >:: 
  (fun _ -> assert_raises 
      (Mp_battle.PlayerNotFound("Borat"))  
      (fun () -> current_health btl "Borat"));

  mp_current_pp_test "mp initial pp of Aang move 1" btl "Aang" 1 15;
  "name does not exist" >:: 
  (fun _ -> assert_raises 
      (Mp_battle.PlayerNotFound("Borat"))  
      (fun () -> current_pp btl "Borat" 1));

  mp_new_health_test "mp Aang attacks Ty Lee w/ move 1" btl "Aang" 1 "Ty Lee" 85.0;
  "name does not exist" >:: 
  (fun _ -> assert_raises 
      (Mp_battle.PlayerNotFound("Borat")) 
      (fun () -> new_health btl "Borat" 1 "Boop"));

  mp_current_pp_test "mp Aang pp after move 1 used once" btl2 "Aang" 1 14;

  mp_current_health_test "mp post-move health Aang" btl2 "Aang" 100.0;
  mp_current_health_test "mp post-move health Ty Lee" btl2 "Ty Lee" 85.0;
  "mp aang move 69 should not exist" >:: (fun _ -> 
      assert_equal
        (mp_make_move btl2 "Aang" 69 "Ty Lee") 
        (IllegalInvalidMove));

]

let suite =
  "test suite for final proj"  >::: List.flatten [
    characters_tests; 
    battle_tests;
    mp_battle_tests;
  ]

let _ = run_test_tt_main suite