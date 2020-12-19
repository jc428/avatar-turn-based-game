open Mp_battle



let pause () = 
  print_endline "\n\n Press anything to continue. ";
  print_string "|>>";
  match read_line () with 
  | _ -> ()

let print_characters lst = 
  let rec helper lst' = 
    match lst' with 
    | [] -> ()
    | h :: t -> begin
        let id = Mp_character.id h in 
        let name = Mp_character.id_to_name id in 
        print_string ("\n" ^ (string_of_int id) ^ ". "
                      ^ name ^
                      ": " ^ Mp_character.c_description name);
        helper t 
      end
  in 
  helper lst 

let rec print_moves btl chr (moves:Mp_character.move list) =
  match moves with
  | [] -> ()
  | h :: t -> begin
      let pp = Mp_battle.current_pp btl chr h.id in
      print_string ("\n" ^ (string_of_int h.id) ^ ". " ^ (h.m_name) ^
                    " Damage: " ^ (string_of_float h.damage) ^
                    " PP: " ^ (string_of_int pp)
                    ^ "\n" ^ h.m_description);
      print_moves btl chr t
    end

let char_str = "\nPlease enter one of the chracters listed above as a \
                number (i.e. 1) \n"

let target_str = "\nEnter the number of the character you wish to target using \
                  this move. \n"

let health (battle: Mp_battle.t) (name : string) =
  Mp_battle.current_health battle name

let health_str battle name = 
  health battle name |> string_of_float

let dead battle player =
  health battle player <= 0.

let dead_list battle pl_list =
  List.filter (dead battle) pl_list

let winner battle pl1 pl2 pl3 pl4 = 
  if (dead battle pl1 && dead battle pl2) then [3;4]
  else if (dead battle pl3 && dead battle pl4) then [1;2]
  else (* will return list of dead players *)
    let rec helper acc = function
      | [] -> []
      | h :: t -> if h = pl1 then acc @ [1]
        else if h = pl2 then acc @ [2]
        else if h = pl3 then acc @ [3]
        else acc @ [4]
    in
    helper [] (dead_list battle [pl1; pl2; pl3; pl4])

let print_battle_state battle pl1 pl2 pl3 pl4 = 
  print_string ("\n" ^ pl1 ^ "'s health: " ^ (health_str battle pl1));
  print_string ("\n" ^ pl2 ^ "'s health: " ^ (health_str battle pl2));
  print_string ("\n" ^ pl3 ^ "'s health: " ^ (health_str battle pl3));
  print_string ("\n" ^ pl4 ^ "'s health: " ^ (health_str battle pl4) ^ "\n")

(** [select_character lst str] returns the name of character with the id 
    identified by the user input*)
let rec select_character lst str = 
  match read_int_opt () with 
  | None ->begin 
      match str with
      | a -> 
        print_string a;
        print_string "|>>";
        select_character lst str
    end
  | Some i -> begin 
      try 
        (if not (List.mem (Mp_character.id_to_name i) lst) then begin
            print_string "\nNot a valid option! \
                          Try one of the choices listed above. \n";
            print_string "|>>";
            select_character lst str
          end
         else Mp_character.id_to_name i) 
      with 
      | Mp_character.UnknownId i -> begin 
          print_string "\nNot a valid option! \
                        Try one of the choices listed above. \n";
          print_string "|>>";
          select_character lst str
        end
    end

let select_players () = 
  let characters = ref Mp_character.characters in
  print_string "\n\nEnter the number of Player 1's character: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player1 = select_character (Mp_character.names !characters) char_str in 
  characters := Mp_character.remove_character player1 !characters;
  print_string ("\nPlayer 1 playing as " ^ player1 );
  print_string "\nEnter the number of Player 2's character: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player2 = select_character (Mp_character.names !characters) char_str in 
  characters := Mp_character.remove_character player2 !characters;
  print_string ("\nPlayer 2 playing as " ^ player2 );
  print_string "\n\nEnter the number of Player 3's character: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player3 = select_character (Mp_character.names !characters) char_str in 
  characters := Mp_character.remove_character player3 !characters;
  print_string ("\nPlayer 3 playing as " ^ player3 );
  print_string "\n\nEnter the number of Player 4's character: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player4 = select_character (Mp_character.names !characters) char_str in
  characters := Mp_character.remove_character player1 !characters; 
  print_string ("\nPlayer 4 playing as " ^ player4 );
  [player1; player2; player3; player4]

let start_battle battle = 
  let players = Mp_battle.players battle in
  let pl1 = List.hd players in
  let pl2 = List.nth players 1 in
  let pl3 = List.nth players 2 in
  let pl4 = List.nth players 3 in
  let rec fight battle_st =
    let rec player_turn btl x = 
      let player = if not (dead battle (List.nth players x)) 
        then List.nth players x
        else begin
          if x < 2 
          then List.nth players (List.hd (List.filter (fun y -> y != x) [0;1]))
          else 
            List.nth players (List.hd (List.filter (fun y -> y != x) [2;3]))
        end
      in
      print_string ("\n\nPlayer " ^ string_of_int (x+1) ^ 
                    "'s turn- make a move!");
      print_moves battle_st player (Mp_battle.player_moves btl player);
      print_string "\n|>>";
      match (read_int_opt ())with
      | None -> begin
          print_string "\nPlease enter one of the moves listed above as a \
                        number (i.e. 1) \n";
          print_string "|>>";
          fight battle_st
        end
      | Some i -> begin
          let rec chars acc lst =
            match lst with
            | [] -> acc
            | a :: t -> if List.mem (Mp_character.name a) players 
              then chars (a :: acc) t else chars acc t
          in let c = List.rev (chars [] Mp_character.characters) in
          print_characters c;
          print_string target_str;
          print_string "\n |>> ";
          let target = select_character players target_str in
          match Mp_battle.mp_make_move battle_st player i target with
          | Legal battle_nxt -> begin
              let winner = winner battle_nxt pl1 pl2 pl3 pl4 in
              if (winner = []) then begin
                print_battle_state battle_nxt pl1 pl2 pl3 pl4;
                player_turn battle_nxt (if x = 0 || x = 1 then x + 2 else x - 1)
              end
              else begin
                let line = begin
                  match winner with
                  | [a] -> "\nOh no! " ^ List.nth players (a-1) ^ "has been \
                                                                   defeated!"
                  | [a;b] -> begin
                      if (a = 3 && b = 4) || (a = 1 && b = 2) then begin
                        if (x + 1 = a || x + 1 = b) then
                          "\nCongratulations " ^ List.nth players (a-1) ^ 
                          " and " ^ List.nth players (b-1) ^ "! Your team won!"
                        else "\nYour team lost! " ^ List.nth players (a-1) ^ 
                             " and " ^ List.nth players (b-1) ^ " won!"
                      end
                      else "\nOh no! " ^ List.nth players (a-1) ^ "and " ^
                           List.nth players (b-1) ^ "have been defeated!"
                    end
                  | _ -> "impossible"
                end
                in print_string line;
                if List.length winner = 1 || 
                   (winner != [1;2] && winner != [3;4])
                then begin
                  print_battle_state battle_nxt pl1 pl2 pl3 pl4;
                  player_turn battle_nxt (if x = 0 || x = 1 then x + 2 else x - 1)
                end
                else
                  pause ();
                print_endline "\n Would you like to play again?: \n 
                 1. Yes \n 
                 2. Quit  \n";
                print_string "|>>"; 
                let rec helper () = 
                  match (read_int_opt ()) with
                  | None -> begin
                      print_string "\nPlease enter one of the options listed \
                                    above as a number (i.e. 1) \n";
                      print_string "|>>";
                      helper ()
                    end
                  | Some 1 -> begin 
                      fight battle
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
    player_turn battle_st 0
  in 
  fight battle

let play_mp_game () =
  print_string "\nA 2 vs 2 team battle";
  pause ();
  print_string "\n\nChoose your fighter "; 
  let players = select_players () in 
  let battle = mp_init_battle players in 
  start_battle battle





