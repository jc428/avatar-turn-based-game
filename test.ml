open OUnit2
open Characters
open Battle
open Mp_battle
(* We used this test suite to test the modules characters, battle, 
   mp_character, and mp_battle. We tested to ensure that the intended
   states and values were being returned by the functions in these modules so
   that we could be confident that the character properties (health,
   moves, etc.) and battle state (which battle, the winner) were gets updated\
   correctly. We used glassbox testing to check for all of the exceptions, 
   as although they aren't always included in the specification, we wanted
   to be sure that the exceptions get raised appropriately. These exceptions
   get caught and handled in the sp_play and mp_play modules as part of the 
   game sequence, so it was important that we verify this part of our code. 
   The modules main, sp_play, and mp_play, which runs the game, was tested
   interactively throughout development. As the functions in these modules
   all return () and executes the game in the terminal, it only makes 
   sense to test these modules interactively by running make play and playing 
   through the battles. *)

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

    update_moves_test "swap move 1 with 5" ba "Aang" 1 5 
      [{
        id = 5;
        is_super =  false;
        m_name = "Fire breath";
        m_element = Fire;
        m_description = "Aang harnesses his inner dragon, unleashing a breath of fire";
        damage = 8.0;
        pp = 25;
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

    update_stats_test "multiply Aang health by 20%" ba "Aang" "health" 1.2 
      None {health = 120.; power = 1.0; speed = 1.0; evasiveness = 1.0};

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

open Mp_character 
(* multiplayer character test *)
let mp_names_test 
    (test_name : string)
    (char_list : Mp_character.t list)
    (expected_output : name list ) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (names char_list) )

let is_character_test
    (test_name : string)
    (name : string)
    (expected_output : bool) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (is_character name) ~printer: string_of_bool)

let id_to_name_test
    (test_name : string)
    (id : int)
    (expected_output : name) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (id_to_name id))

let c_description_test 
    (test_name : string)
    (name : string)
    (expected_output : string) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (c_description name))

let c_stats_test
    (test_name : string)
    (name : string)
    (expected_output : Mp_character.stats) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (c_stats name))

let c_moves_test 
    (test_name : string)
    (name : string)
    (expected_output : move list) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (c_moves name))

let remove_character_test 
    (test_name : string)
    (name : string)
    (lst : t list)
    (expected_output : name list) : test =
  test_name >:: (fun _ -> 
      assert_equal expected_output (names (remove_character name lst)))

let mp_character_tests = [
  mp_names_test "names of empty list" [] [];
  mp_names_test "names of all characters" characters
    ["Aang"; "Zuko"; "Ty Lee"; "Katara"; "Sokka"; "Toph"; "Suki"; "Mai"];
  is_character_test "ty_lee is a character" "Ty Lee" true;
  is_character_test "appa is not a character" "appa" false;
  id_to_name_test "7 is the id of suki" 7 "Suki";
  "no character with id 9 exists" >:: 
  (fun _ -> assert_raises 
      (UnknownId 9) 
      (fun () -> id_to_name 9));
  c_description_test "toph description" "Toph"
    "small, blind, sees with her feet";
  c_stats_test "stats of toph" "Toph" 
    {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  c_moves_test "toph moves" "Toph"
    [{
      id = 1; is_super = false; m_name = "Avalanche";
      m_element = Earth;
      m_description = "Toph creates an avalanche, crushing her opponent with \
                       large rocks";
      damage = 25.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Little Rich Girls";
       m_element = Normal;
       m_description =  "Toph uses her wealthy background to hire a random guy \
                         to punch her opponent in the face";
       damage = 15.0;
       pp = 20
     };
     {
       id = 3; is_super = false; m_name = "Blind Healing";
       m_element = Earth;
       m_description =  "Toph heals a teammate with her blind senses";
       damage = -13.0;
       pp = 12
     };
     {
       id = 4; is_super = false; m_name = "Geyser";
       m_element = Water;
       m_description =  "Toph creates a hole in the ground and a geyser erupts \
                         in her opponent's face";
       damage = 22.0;
       pp = 13
     }];
  remove_character_test "remove Aang" "Aang" characters 
    ["Zuko"; "Ty Lee"; "Katara"; "Sokka"; "Toph"; "Suki"; "Mai"];
  remove_character_test "remove Appa, not in characters" "Appa" characters 
    ["Aang"; "Zuko"; "Ty Lee"; "Katara"; "Sokka"; "Toph"; "Suki"; "Mai"];
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

let team1_test 
    (test_name : string)
    (btl : Mp_battle.t)
    (expected_output : name * name) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (team1 btl))

let team2_test 
    (test_name : string)
    (btl : Mp_battle.t)
    (expected_output : name * name) : test = 
  test_name >:: (fun _ -> 
      assert_equal expected_output (team2 btl))

let power_test
    (test_name : string)
    (btl: Mp_battle.t)
    (name: name)
    (expected_output : float) : test = 
  test_name >:: (fun _ ->
      assert_equal expected_output (power btl name))

let speed_test
    (test_name : string)
    (btl: Mp_battle.t)
    (name: name)
    (expected_output : float) : test = 
  test_name >:: (fun _ ->
      assert_equal expected_output (speed btl name))

let evasiveness_test
    (test_name : string)
    (btl: Mp_battle.t)
    (name: name)
    (expected_output : float) : test = 
  test_name >:: (fun _ ->
      assert_equal expected_output (evasiveness btl name))

let mp_extract (legal : Mp_battle.result) = 
  match legal with 
  | Legal t -> t
  | IllegalInvalidMove -> raise (Failure "IllegalInvalidMove")
  | IllegalNoPP -> raise (Failure "IllegalNoPP")

let btl = mp_init_battle ["Aang";"Zuko";"Ty Lee";"Katara"] 
let btl2_raw = mp_make_move btl "Aang" 1 "Ty Lee"
let btl2 = mp_extract btl2_raw

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
  team1_test "team1 members are Aang and Zuko" btl ("Aang", "Zuko");
  team2_test "team2 members are Ty Lee and Katara" btl ("Ty Lee", "Katara");
  power_test "Ty Lee has power 1." btl "Ty Lee" 1.0;
  speed_test "Ty Lee has speed 1." btl "Ty Lee" 1.0;
  evasiveness_test "Ty Lee has evasiveness 1." btl "Ty Lee" 1.0;
]

let suite =
  "test suite for final proj"  >::: List.flatten [
    characters_tests; 
    battle_tests;
    mp_battle_tests;
    mp_character_tests;
  ]

let _ = run_test_tt_main suite