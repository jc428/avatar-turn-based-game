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

(** [select_character lst] returns the name of character with the id 
    identified by the user input*)
let rec select_character lst = 
  match read_int_opt () with 
  | None ->begin 
      print_string "\nPlease enter one of the chracters listed above as a \
                    number (i.e. 1) \n";
      print_string "|>>";
      select_character lst 
    end
  | Some i -> begin 
      try (Mp_character.id_to_name i) with 
      | Mp_character.UnknownId i -> begin 
          print_string "\nNot a valid option! \
                        Try one of the choices listed above. \n";
          print_string "|>>";
          select_character lst 
        end
    end

let select_players () = 
  let characters = ref Mp_character.characters in
  print_string "\n\nEnter the number of Player 1's charcter: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player1 = select_character !characters in 
  characters := Mp_character.remove_character player1 !characters;
  print_string ("\nPlayer 1 playing as " ^ player1 );
  print_string "\n1nEnter the number of Player 2's charcter: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player2 = select_character !characters in 
  characters := Mp_character.remove_character player2 !characters;
  print_string ("\nPlayer 2 playing as " ^ player2 );
  print_string "\n1nEnter the number of Player 3's charcter: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player3 = select_character Mp_character.characters in 
  characters := Mp_character.remove_character player3 !characters;
  print_string ("\nPlayer 3 playing as " ^ player3 );
  print_string "\n\nEnter the number of Player 4's charcter: ";
  print_characters !characters;
  print_string "\n |>> ";
  let player4 = select_character Mp_character.characters in
  characters := Mp_character.remove_character player1 !characters; 
  print_string ("\nPlayer 4 playing as " ^ player4 );
  [player1; player2; player3; player4]

let player_order btl = 
  let fastest_teammember team = 
    match team with 
      (a, b) -> begin 
        let speedA = Mp_battle.speed a in 
        let speedB = Mp_battle.speed b in 
        if (speedA > speedB) then (a, speedA)
        else (b, speedB)
      end



let start_battle btl = 
  ()

let play_mp_game () =
  print_string "\nA 2 vs 2 team battle";
  pause ();
  print_string "\n\nChoose your fighter "; 
  let players = select_players () in 
  let battle = init_battle players in 
  start_battle battle





