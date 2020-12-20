exception DoneWithAscii
open Episode 

let start_from_save () = ()

let read_ascii filename =
  let ic = open_in filename in
  let rec process_line () =
    let line = try input_line ic 
      with End_of_file -> raise DoneWithAscii
    in
    print_endline line;
    process_line ();
  in process_line ()

let pause () = 
  print_endline "\n\n Press anything to continue. ";
  print_string "|>>";
  match read_line () with 
  | _ -> ()

let rec print_moves btl chr (moves:Characters.move list) =
  (* sort moves before; in case a previously learned move is out of order *)
  let moves = List.sort (fun (m1:Characters.move) m2 -> m1.id - m2.id) moves in
  match moves with
  | [] -> ()
  | h :: t -> begin
      let pp = Battle.get_current_pp btl chr h.id in
      print_string ("\n" ^ (string_of_int h.id) ^ ". " ^ (h.m_name) ^
                    "\nElement: " ^ (Characters.string_of_element h.m_element) ^
                    "\nDamage: " ^ (string_of_float h.damage) ^
                    " PP: " ^ (string_of_int pp)
                    ^ "\n" ^ h.m_description ^ "\n");
      print_moves btl chr t
    end

let print_new_moves (new_moves: Characters.move list) = 
  let h = List.hd new_moves in
  print_string ("\n" ^ (string_of_int h.id) ^ ". " ^ (h.m_name) ^
                " Damage: " ^ (string_of_float h.damage) ^
                " PP: " ^ (string_of_int h.pp)
                ^ "\n" ^ h.m_description)

let print_stats (stats : Characters.stats) =
  begin
    print_string (" Health: " ^ (string_of_float stats.health) ^
                  " Power: " ^ (string_of_float stats.power) ^
                  " Speed: " ^ (string_of_float stats.speed) ^
                  " Evasiveness " ^ (string_of_float stats.evasiveness));
  end

let health (battle: Battle.battle) (name : string) =
  Battle.get_current_health battle name

let health_str battle name = 
  health battle name |> string_of_float

let make_hp_bar hp : unit =
  let rec make_hp_bar_helper num_bars acc = 
    match num_bars with 
    | num when num > 0 -> let new_acc = "|" ^ acc in 
      make_hp_bar_helper (num_bars - 1) new_acc
    | num when num <= 0 -> acc
    | _ -> failwith "this should never happen"
  in
  let num_bars = 
    (match hp / 5 with
     | 0 -> 1
     | not_zero -> not_zero)
  in
  match num_bars with
  | n when n < 5 -> ANSITerminal.(print_string [red] 
                                    (make_hp_bar_helper num_bars ""))
  | n when n < 10 -> ANSITerminal.(print_string [yellow] 
                                     (make_hp_bar_helper num_bars ""))
  | _ -> ANSITerminal.(print_string [green] (make_hp_bar_helper num_bars ""))

let print_battle_state battle ch1 ch2 = 
  let make_hp_bar hp : unit =
    let rec make_hp_bar_helper num_bars acc = 
      match num_bars with 
      | num when num > 0 -> let new_acc = "|" ^ acc in 
        make_hp_bar_helper (num_bars - 1) new_acc
      | num when num <= 0 -> acc
      | _ -> failwith "this should never happen"
    in
    let num_bars = 
      (match hp / 5 with
       | 0 -> 1
       | not_zero -> not_zero)
    in
    match num_bars with
    | n when n < 5 -> ANSITerminal.(print_string [red] 
                                      (make_hp_bar_helper num_bars ""))
    | n when n < 10 -> ANSITerminal.(print_string [yellow] 
                                       (make_hp_bar_helper num_bars ""))
    | _ -> ANSITerminal.(print_string [green] (make_hp_bar_helper num_bars ""))
  in
  let player_hp = health_str battle ch1 in
  let opponent_hp = health_str battle ch2 in
  print_string ("\nPlayer health: " ^ player_hp ^ "\n");
  make_hp_bar (int_of_float (float_of_string player_hp));
  print_string ("\nOpponent health: " ^ opponent_hp ^ "\n");
  make_hp_bar (int_of_float (float_of_string opponent_hp))

let winner battle ch1 ch2 = 
  if (health battle ch1 <= 0.) then ch2
  else if (health battle ch2 <= 0.) then ch1
  else ""

let print_player_dialogue d = 
  let rec helper c =
    if (c < Array.length d) then begin
      print_string ("\n" ^ (string_of_int (c + 1)) ^ ". " ^ d.(c));
      helper (c+1)
    end
    else
      print_string ""
  in 
  helper 0 ;
  print_string "\n|>>"


let player_response player ep = 
  let d = Episode.player_dialogue ep in
  print_player_dialogue d;
  let rec player_response_r () = 
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        player_response_r ()
      end
    | Some 1 -> print_string ("\n" ^ player ^ ": " ^ d.(0))
    | Some 2 -> print_string ("\n" ^ player ^ ": " ^d.(1))
    | Some i -> begin
        print_string "\nNot a valid option! \
                      Try one of the choices listed above. \n";
        print_string "|>>";
        player_response_r ()
      end
  in
  player_response_r ()

let print_enemy_line ep enemy i = 
  print_string ("\n\n" ^ enemy ^ ": " ^ (Episode.enemy_line ep i) ^ "\n")

let battle_intro player characters battle enemy ep =
  print_string ("\nPlaying as " ^ player ^
                "\n" ^ (Characters.get_c_description characters player) ^
                "\n\nAgainst " ^ enemy ^
                "\n" ^ (Characters.get_c_description characters enemy) ^ "\n");
  try read_ascii ("ascii-" ^ String.lowercase_ascii enemy ^ ".txt")
  with DoneWithAscii ->
    print_enemy_line ep enemy 0;
    player_response player ep;
    print_enemy_line ep enemy 1;
    pause ();
    print_string ("\n\n Player starting health: " ^ (health_str battle player));
    print_string ("\n Opponent starting health: " ^ (health_str battle enemy))

let rec start_episode f i is_from_save = 
  let ep = Episode.from_json f in
  Episode.set_current_battle ep i;
  print_string ("\n " ^ (Episode.intro ep));
  pause ();
  play_battle ep is_from_save 

and play_battle ep is_from_save =
  let rec play_battle_r ep bool = 
    let characters = 
      (match is_from_save with 
       | true -> Episode.get_characters_from_save ep "save_file"
       | false -> Episode.get_characters ep)
    in
    let battle = characters |> Battle.init_battle in 
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
    pause ();
    fight battle battle player characters enemy ep;
    if is_from_save then play_battle_r ep true
    else play_battle_r ep false
  in 
  play_battle_r ep is_from_save

and fight battle battle_st player characters enemy ep =
  let player_turn btl = 
    print_string "\n Player's turn- make a move!";
    print_moves battle_st player (Characters.get_moves characters player);
    print_string "\n|>>";
    let rec move_helper () =
      match (read_int_opt ())with
      | None -> begin
          print_string "\nPlease enter one of the moves listed above as a \
                        number (i.e. 1) \n";
          print_string "|>>";
          move_helper ()
        end
      | Some i -> begin
          match Battle.make_move battle_st player i with
          | Legal battle_nxt -> begin
              print_string 
                ("\n Player used " ^
                 (Characters.get_move_by_id characters player i).m_name);
              print_string 
                ("\n" ^ 
                 (Characters.get_move_description characters player i) ^ 
                 "\n");
              check_winner battle characters battle_nxt
                player enemy ep battle_st false
            end
          | IllegalInvalidMove -> begin
              print_string "\nNot a valid move! Try one of the moves \
                            listed above!\n";
              print_string "|>>";
              move_helper ()
            end
          | IllegalNoPP -> begin
              print_string "\nYou've exhausted this move! Try another move \
                            listed above!\n";
              print_string "|>>";
              move_helper ()
            end
          | IllegalStat -> failwith "IllegalStat (impossible)"
        end
    in move_helper ()
  in
  player_turn battle_st

and check_winner
    battle characters battle_nxt player enemy ep battle_st player_nxt = 
  let winner = winner battle_nxt player enemy in
  if (winner = "") then begin
    print_battle_state battle_nxt player enemy;
    if player_nxt then 
      fight battle_st battle_nxt player characters enemy ep
    else
      enemy_turn battle_nxt enemy characters player ep battle_st
  end
  else if (winner = player) then begin
    print_enemy_line ep enemy 2;
    print_string ("\n" ^ (Episode.outro ep true));
    pause ();
    let rec user_input_move () = 
      print_string "\n\n You have unlocked a new move."; 
      print_new_moves (Characters.get_new_moves characters);
      pause ();
      print_moves battle player 
        (Characters.get_moves characters player);
      print_string 
        "\nEnter the number for the move you would like to \
         replace\n or enter -1 if you wish to keep your \
         current moves:\n";
      print_string "\n|>>";
      let rec old_move_id_helper () =
        try (read_int ()) with 
        | Failure _ ->
          print_string "\nPlease enter one of the moves listed \
                        above as a number (i.e. 1) \n";
          print_string "\n|>>";
          old_move_id_helper () in
      let old_move_id = old_move_id_helper () in
      if old_move_id != -1 then begin
        print_stats (Characters.get_stats characters player);
        print_string "\n Enter a stat to upgrade: ";
        print_string "\n|>>";
        let stat = read_line () in
        let lower_stat = String.lowercase_ascii stat in
        let res = Battle.battle_end
            battle_st player old_move_id 5 lower_stat 1.2 None
        in
        match res with
        | Legal final_ba -> Save.write final_ba characters;
        | IllegalInvalidMove -> begin
            print_string "\nInvalid input(s)! \
                          Please try again.";
            pause ();
            user_input_move () end
        | IllegalStat -> begin 
            print_string "\nInvalid input(s)! \
                          Please try again.";
            pause ();
            user_input_move () end
        | IllegalNoPP -> begin
            print_string "\nIllegalNoPP (impossible)";
            pause ();
            user_input_move()
          end
      end
      else pause ();
    in
    user_input_move ();
    continue ep true;
  end
  else begin
    print_enemy_line ep enemy 3;
    print_string ("\n" ^ (Episode.outro ep false));
    continue ep false;
  end

and enemy_turn btl enemy characters player ep battle_st =
  let ai_dummy_move ba =
    Random.int (List.length (Battle.get_enemy_moves ba)) + 1
  in
  let x = ai_dummy_move btl in
  let battle_nxt =
    match Battle.make_move btl enemy x with
    | Legal battle_nxt -> battle_nxt
    | IllegalInvalidMove -> btl
    | IllegalNoPP -> btl
    | IllegalStat -> failwith "IllegalStat (impossible)"
  in
  pause ();
  print_string ("\n Opponent turn- " ^ enemy ^ " used " ^
                (Characters.get_move_by_id characters enemy x).m_name);
  print_string ("\n" ^ 
                (Characters.get_move_description characters enemy x) ^ 
                "\n");
  print_battle_state battle_nxt player enemy;

  pause ();
  check_winner btl characters battle_nxt player enemy ep battle_st true

and continue ep next = 
  print_endline "\n Would you like to keep playing?: \n 
                 1. Continue \n 
                 2. Quit  \n";
  print_string "|>>"; 
  let rec helper () = 
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        helper ()
      end
    | Some 1 -> begin 
        if next then 
          if (Episode.move_to_next_episode ep) then begin
            print_string ("\nStarting next episode: " ^ 
                          Episode.next_episode ep );
            start_episode (Episode.next_episode ep) 1 true
          end
          else play_battle (next_battle ep) true
        else play_battle ep true
      end
    | Some 2 -> begin
        print_endline "\nSee you next time!";
        exit 0
      end
    | Some i -> begin
        print_string "\nNot a valid option! \
                      Try one of the choices listed above. \n";
        print_string "|>>";
        helper ()
      end
  in helper ()

let select_episode () = 
  print_endline "\n Select one of the following: \n 
                 1. Episode 1: Team Avatar\n 
                 2. Episode 2: Water Tribe\n
                 3. Episode 3: Earth Kingdom\n
                 4. Episode 4: Fire Nation\n
                 5. Quit  \n";
  print_string "|>>";         
  let rec select_battle_r () = 
    match (read_int_opt ()) with
    | None -> begin
        print_string "\nPlease enter one of the options listed above as a \
                      number (i.e. 1) \n";
        print_string "|>>";
        select_battle_r ()
      end
    | Some 1 -> start_episode "Ep1dialogue.json" 1 false
    | Some 2 -> start_episode "Ep2dialogue.json" 1 false
    | Some 3 -> start_episode "Ep3dialogue.json" 1 false
    | Some 4 -> start_episode "Ep4dialogue.json" 1 false
    | Some 5 -> begin
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
  try read_ascii "ascii-aang.txt" with DoneWithAscii ->
    start_sp ()