open Sys
open Yojson
open Characters
open Battle

(* Declares and sets json values
   [> `Assoc of (string * [> `Assoc of (string * [> `String of string ]) list ]) list ] 
*)
let write (ba:Battle.battle) (ch:Characters.t) : unit = 
  let move_list = Battle.get_player_moves ba in
  let first = List.nth move_list 0 in
  let second = List.nth move_list 1 in
  let third = List.nth move_list 2 in
  let fourth = List.nth move_list 3 in
  let string_of_element elt =
    match elt with 
    | Air -> "Air "
    | Fire -> "Fire"
    | Earth -> "Earth"
    | Water -> "Water"
    | Avatar -> "Avatar"
    | _ -> "Bruh"
  in
  let rec get_new_move_pp new_move_list first_id = 
    match new_move_list with
    | h :: t -> if h.id = first.id then h.pp else get_new_move_pp t first_id
    | [] -> failwith "fucks up at get_new_move_pp for some reason"
  in
  let p_name = List.hd (get_names ch) in
  let save =
    `Assoc [("stats", (`Assoc [
        ("health", `Float (get_current_health ba (List.hd (get_names ch)))); 
        ("power", `Float (get_power ba (List.hd (get_names ch))));
        ("speed", `Float (get_speed ba (List.hd (get_names ch))));
        ("evasiveness", `Float (get_evasiveness ba (List.hd (get_names ch))))
      ]
      )
      );

       ("moves", (`List [
           `Assoc [
             ("id", `Int first.id);
             ("name", `String first.m_name);
             ("issuper", `Bool first.is_super);
             ("element", `String (string_of_element first.m_element));
             ("description", `String first.m_description);
             ("damage", `Float first.damage);
             ("pp", `Int (get_new_move_pp (get_new_moves ch) first.id))
           ]; 
           `Assoc [
             ("id", `Int second.id);
             ("name", `String second.m_name);
             ("issuper", `Bool second.is_super);
             ("element", `String (string_of_element second.m_element));
             ("description", `String second.m_description);
             ("damage", `Float second.damage);
             ("pp", `Int (get_move_by_id ch p_name second.id).pp)
           ];
           `Assoc [
             ("id", `Int third.id);
             ("name", `String third.m_name);
             ("issuper", `Bool third.is_super);
             ("element", `String (string_of_element third.m_element));
             ("description", `String third.m_description);
             ("damage", `Float third.damage);
             ("pp", `Int (get_move_by_id ch p_name third.id).pp)
           ];
           `Assoc [
             ("id", `Int fourth.id);
             ("name", `String fourth.m_name);
             ("issuper", `Bool fourth.is_super);
             ("element", `String (string_of_element fourth.m_element));
             ("description", `String fourth.m_description);
             ("damage", `Float fourth.damage);
             ("pp", `Int (get_move_by_id ch p_name fourth.id).pp)
           ];
         ]
       )
       )
      ]
  in 
  Yojson.Basic.to_file "save_file.json" save


(* 

(*ascii demo*)

exception DoneWithAscii
let read filename =
  let ic = open_in filename in
  let rec process_line () =
    let line = try input_line ic with End_of_file -> raise DoneWithAscii
    in
       print_endline line;
       process_line ();
in process_line ()

let () = try read "aang-non-battle.txt" with DoneWithAscii -> () 

*)

