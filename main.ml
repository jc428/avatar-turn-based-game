exception DoneWithAscii
open Episode 

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
                     - a text based game\nFull screen terminal for the best \
                     experience :) \n");
    print_endline "Select the play mode: \n
                  1. Single Player: Travel through the Element Nations as Aang\n 
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
      | Some 1 -> Sp_play.play_sp_game ()
      | Some 2 -> Mp_play.play_mp_game ()
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