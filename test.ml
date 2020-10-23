open OUnit2
open Characters
open Battle

let ch = Characters.from_json "MS1satisfactory"
let bat = Battle.init_battle ch

let make_move_test name make_move battle move expected_output: test =
  name >:: (fun _ ->
      assert_equal expected_output (make_move battle move))

let battle_tests =
  [
    "invalid move" >:: 
    (fun _ -> assert_raises (Failure "IllegalInvalidMove") 
        (fun () -> make_move bat "cockbending"));
  ]

let suite =
  "test suite for avatar-turn-based-game"  >::: List.flatten [
    battle_tests;
  ]

let _ = run_test_tt_main suite