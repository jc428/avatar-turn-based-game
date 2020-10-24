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

let get_current_health (ba : battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_health
  else if name = (my_list_taili (get_names ba.characters)) then ba.enemy_health
  else failwith "name does not belong to player or enemy"

let get_current_pp ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (Characters.get_move_by_id ba.characters name move_id).pp
  else
    failwith "name not found"

let set_new_health ba user_name (user_move_id : int) : float =
  if user_name = (my_list_hd (get_names ba.characters)) then
    let enemy_name = my_list_taili (get_names ba.characters) in 
    let user_move = get_move_by_id ba.characters user_name user_move_id in
    let cur_enemy_health = get_current_health ba enemy_name in
    cur_enemy_health -. user_move.damage
  else if user_name = (my_list_taili (get_names ba.characters)) then 
    let player_name = my_list_hd (get_names ba.characters) in 
    let enemy_move = get_move_by_id ba.characters player_name user_move_id in
    let cur_player_health = get_current_health ba player_name in
    cur_player_health -. enemy_move.damage
  else failwith "name does not belong to player or enemy"

let set_new_pp ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (get_current_pp ba name move_id) - 1
  else
    failwith "name not found"

type result = Legal of t | IllegalInvalidMove | IllegalNoPP

let change_pp ba name move_id : move list= 
  let filtered_move_list = 
    List.filter (fun m -> m.id <> move_id) ba.player_moves in
  let move_to_be_updated = get_move_by_id ba.characters name move_id in
  let updated_pp_move = {
    id = move_to_be_updated.id;
    is_super = move_to_be_updated.is_super;
    m_name = move_to_be_updated.m_name;
    m_element = move_to_be_updated.m_element;
    m_description = move_to_be_updated.m_description;
    damage = move_to_be_updated.damage;
    pp = set_new_pp ba name move_id
  }
  in updated_pp_move :: filtered_move_list

let make_move ba name move_id =
  if (List.mem move_id (List.map (fun m -> m.id) ba.player_moves) = false)
  then IllegalInvalidMove 
  else if get_current_pp ba name move_id = 0
  then IllegalNoPP
  else if name = my_list_hd (get_names ba.characters) then
    Legal {
      characters = ba.characters;
      player_health = ba.player_health;
      enemy_health = set_new_health ba name move_id;
      player_moves = change_pp ba name move_id
    }
  else
    Legal {
      characters = ba.characters;
      player_health = set_new_health ba name move_id;
      enemy_health = ba.enemy_health;
      player_moves = change_pp ba name move_id
    }

