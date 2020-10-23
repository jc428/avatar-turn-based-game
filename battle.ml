open Characters

type battle = {
  characters : t;
  player_health : float;
  enemy_health : float;
  player_moves : move list;
}

let init_battle ch = {
  characters = ch;
  player_health = (get_stats ch (List.hd (get_names ch))).health;
  enemy_health = (get_stats ch (List.hd (List.tl (get_names ch)))).health;
  player_moves = (get_moves ch (List.hd (get_names ch)));
}

let health_helper =
  failwith "todo"

let get_current_player_health battle player = 
  failwith "todo" 

let get_current_enemy_health battle player =
  failwith "todo"

let get_current_pp battle player =
  failwith "todo"

let set_new_player_health battle player =
  failwith "todo"

let set_new_enemy_health battle player =
  failwith "todo"

let set_new_pp battle player move =
  failwith "todo"

type result = Legal of t | IllegalInvalidMove | IllegalNoPP

let make_move battle move =
  if (List.mem move (List.map (fun m -> m.m_name) battle.player_moves) = false)
  then IllegalInvalidMove 
  else IllegalNoPP

(* 

  else if
else
  set_new_player_health
    set_new_enemy_health
    set_new_pp *)