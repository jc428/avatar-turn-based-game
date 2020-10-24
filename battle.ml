open Characters

type battle = {
  characters : t;
  player_health : float;
  enemy_health : float;
  player_moves : move list;
}

type t = battle

let init_battle ch = {
  characters = ch;
  player_health = (get_stats ch (List.hd (get_names ch))).health;
  enemy_health = (get_stats ch (List.hd (List.tl (get_names ch)))).health;
  player_moves = (get_moves ch (List.hd (get_names ch)));
}

(* let health_helper =
   failwith "todo" *)
let my_list_hd lst = 
  match lst with
  | h :: t -> h 
  | _ -> failwith "bruh"

let rec my_list_taili lst = 
  match lst with
  | h :: [] -> h
  | h :: t -> my_list_taili t
  | _ -> failwith "bruh"

let get_current_health (ba:battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_health
  else if name = (my_list_taili (get_names ba.characters)) then ba.enemy_health
  else failwith "name does not belong to player or enemy"

let get_current_pp ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (Characters.get_move_by_id ba.characters name move_id).pp
  else
    failwith "name not found"

(* let set_new_player_health battle player =
   failwith "todo" *)

(* let set_new_enemy_health battle player =
   failwith "todo" *)

(* let set_new_pp battle player move =
   failwith "todo" *)

(* type result = Legal of t | IllegalInvalidMove | IllegalNoPP

   let make_move battle move =
   if (List.mem move (List.map (fun m -> m.m_name) battle.player_moves) = false)
   then IllegalInvalidMove 
   else IllegalNoPP *)

(* 

  else if
else
  set_new_player_health
    set_new_enemy_health
    set_new_pp *)