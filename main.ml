exception DoneWithAscii

let play_mp_game () =
  ()

let start_from_save () =
  ()

let rec print_moves (moves:Characters.move list) =
  match moves with
  | [] -> ()
  | h :: t -> begin
      print_string ("\n" ^ (string_of_int h.id) ^ ". " ^ (h.m_name) ^
                    " Damage: " ^ (string_of_float h.damage) ^
                    " PP: " ^ (string_of_int h.pp)
                    ^ "\n" ^ h.m_description);
      print_moves t
    end

let health (battle: Battle.battle) (name : string) =
  Battle.get_current_health battle name

let health_str battle name = 
  health battle name |> string_of_float

let print_battle_state battle ch1 ch2 = 
  print_string ("\nPlayer health: " ^ (health_str battle ch1));
  print_string ("\nOpponent health: " ^ (health_str battle ch2) ^ "\n")

let winner battle ch1 ch2 = 
  if (health battle ch1 <= 0.) then ch2
  else if (health battle ch2 <= 0.) then ch1
  else ""

let print_player_dialogue ep = 
  let rec helper c = function
    | [] -> ()
    | h :: t -> begin
        print_string ("\n" ^ (string_of_int c) ^ ". " ^  h);
        helper (c+1) t
      end
  in
  helper 1 (Episode.player_dialogue ep);
  print_string "\n|>>"

let player_response ep = 
  print_player_dialogue ep;
  let rec player_response_r () = 
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        player_response_r ()
      end
    | Some i -> begin
        if i < (Episode.player_dialogue ep |> List.length) then
          print_string ""
        else begin
          print_string "\nNot a valid option! \
                        Try one of the choices listed above. \n";
          print_string "|>>";
          player_response_r ()
        end
      end
  in
  player_response_r ()

let print_enemy_line ep enemy i = 
  print_string ("\n\n" ^ enemy ^ ": " ^ (Episode.enemy_line ep i) ^ "\n")

let battle_intro player characters battle enemy ep =
  print_string ("\n Playing as " ^ player ^
                "\n" ^ (Characters.get_c_description characters player) ^
                "\n\n against " ^ enemy ^
                "\n" ^ (Characters.get_c_description characters enemy));
  print_enemy_line ep enemy 0;
  player_response ep;
  print_enemy_line ep enemy 1;
  print_string ("\n\n Player starting health: " ^ (health_str battle player));
  print_string ("\n Opponent starting health: " ^ (health_str battle enemy))

let play_battle str ep =
  let ai_dummy_move ba =
    Random.int (List.length (Battle.get_enemy_moves ba)) + 1
  in
  let characters = Characters.from_json str in
  let battle = characters |> Battle.init_battle in 
  print_string "\n Starting battle... \n";
  let player = 
    match Characters.get_names characters with
    | [] -> ""
    | h :: t -> h
  in 
  let enemy =
    match Characters.get_names characters with
    | [] -> ""
    | h :: t -> List.hd t
  in
  battle_intro player characters battle enemy ep;
  let rec fight battle_st =
    let player_turn btl = 
      let enemy_turn btl =
        let x = ai_dummy_move battle in
        let battle_nxt =
          match Battle.make_move btl enemy x with
          | Legal battle_nxt -> battle_nxt
          | IllegalInvalidMove -> btl
          | IllegalNoPP -> btl
        in
        print_string ("\n Opponent turn- " ^ enemy ^ " used " ^
                      (Characters.get_move_by_id characters enemy x).m_name);
        print_battle_state battle_nxt player enemy;
        fight battle_nxt
      in
      print_string "\n Player's turn- make a move!";
      print_moves (Characters.get_moves characters player);
      print_string "\n|>>";
      match (read_int_opt ())with
      | None -> begin
          print_string "\nPlease enter one of the moves listed above as a \
                        number (i.e. 1) \n";
          print_string "|>>";
          fight battle_st
        end
      | Some i -> begin
          match Battle.make_move battle_st player i with
          | Legal battle_nxt -> begin
              let winner = winner battle_nxt player enemy in
              if (winner = "") then begin
                print_battle_state battle_nxt player enemy;
                enemy_turn battle_nxt
              end
              else if (winner = player) then begin
                print_enemy_line ep enemy 2;
                print_string ("\n" ^ (Episode.outro ep true))
              end
              else begin
                print_enemy_line ep enemy 3;
                print_string ("\n" ^ (Episode.outro ep false))
              end
            end
          | IllegalInvalidMove -> begin
              print_string "\nNot a valid move! Try one of the moves \
                            listed above!\n";
              print_string "|>>";
              fight battle_st
            end
          | IllegalNoPP -> begin
              print_string "\nYou've exhausted this move! Try another move \
                            listed above!\n";
              print_string "|>>";
              fight battle_st
            end
        end
    in
    player_turn battle_st
  in 
  fight battle

let start_episode f = 
  let ep = Episode.from_json f in
  print_string ("\n " ^ (Episode.intro ep));
  play_battle "MS1multiplemoves" ep

let select_episode () = 
  print_endline "\n Select one of the following: \n 
                 1. Episode 1\n 
                 2. Quit  \n";
  print_string "|>>";         
  let rec select_battle_r () = 
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        select_battle_r ()
      end
    | Some 1 -> start_episode "Ep1dialogue.json"
    | Some 2 -> begin
        print_endline "\nSee you next time!";
        exit 0
      end
    | Some i -> begin
        print_string "\nNot a valid option! \
                      Try one of the choices listed above. \n";
        print_string "|>>";
        select_battle_r ()
      end
  in
  select_battle_r ()

let start_sp () = 
  print_endline "\n Select one of the following: \n 
                 1. Start a new episode \n 
                 2. Resume last played episode. \n 
                 3. Quit  \n";
  print_string "|>>";
  let rec start_sp_r () =
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        start_sp_r ()
      end
    | Some 1 -> select_episode ()
    | Some 2 -> start_from_save ()
    | Some 3 -> begin
        print_endline "\nSee you next time!";
        exit 0
      end
    | Some i -> begin
        print_string "\nNot a valid option! \
                      Try one of the choices listed above. \n";
        print_string "|>>";
        start_sp_r ()
      end
  in 
  start_sp_r ()

let play_sp_game () =
  let read filename =
    let ic = open_in filename in
    let rec process_line () =
      let line = try input_line ic 
        with End_of_file -> raise DoneWithAscii
      in
      print_endline line;
      process_line ();
    in process_line ()
  in
  try read "ascii-aang-non-battle.txt" with DoneWithAscii ->
    start_sp ()


(** [main()] prompts for the game to play, then starts it. *)
let main () = 
  let read filename =
    let ic = open_in filename in
    let rec process_line () =
      let line = try input_line ic 
        with End_of_file -> raise DoneWithAscii
      in
      print_endline line;
      process_line ();
    in process_line () 
  in
  try read "ascii-game-title.txt" with DoneWithAscii ->
    ANSITerminal.(print_string [blue]
                    "\n\nWelcome to the Avatar the Last Air Bender \
                     - a text based game\n");
    print_endline "Select the play mode: \n
                  1. Single Player: travel through the Four Nations as Aang \n 
                  2. Multi Player: Battle against your friends as one of \
                   ATLA characters\n
                  3. Quit \n";
    print_string  "|>> ";
    let rec start_game () =
      match (read_int_opt ()) with
      | None -> begin
          print_endline "\nPlease enter one of the options listed above as a \
                         number (i.e. 1). \n";
          print_string "|>>";
          start_game ()
        end
      | Some 1 -> play_sp_game ()
      | Some 2 -> play_mp_game ()
      | Some 3 -> begin
          print_endline "See you next time!";
          exit 0
        end
      | Some i -> begin 
          print_endline "\nNot a valid option! \
                         Try one of the choices listed above. \n";
          print_string "|>>";
          start_game()
        end
    in
    start_game ()

(* Execute the game engine *)
let () = main ()