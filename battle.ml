open Characters

type battle = {
  characters : t;
  player_health : float;
  player_power : float;
  player_speed : float;
  player_evasiveness : float;
  player_moves : move list;
  enemy_health : float;
  enemy_moves : move list;
  enemy_element : element;
}

type t = battle

let init_battle ch = {
  characters = ch;
  player_health = (get_stats ch (List.hd (get_names ch))).health;
  player_moves = (get_moves ch (List.hd (get_names ch)));
  player_power = (get_stats ch (List.hd (get_names ch))).power;
  player_speed = (get_stats ch (List.hd (get_names ch))).speed;
  player_evasiveness = (get_stats ch (List.hd (get_names ch))).evasiveness;
  enemy_health = (get_stats ch (List.hd (List.tl (get_names ch)))).health;
  enemy_moves = (get_moves ch (List.hd (List.tl (get_names ch))));
  enemy_element = (get_c_element ch (List.hd (List.tl (get_names ch))));
}

let my_list_hd lst = 
  match lst with
  | h :: t -> h 
  | _ -> failwith "bruh this input shit is either empty or scuffed idk"

let rec my_list_taili lst = 
  match lst with
  | h :: [] -> h
  | h :: t -> my_list_taili t
  | _ -> failwith "bruh this input shit is either empty or scuffed idk"

let get_current_health (ba : battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_health
  else if name = (my_list_taili (get_names ba.characters)) then ba.enemy_health
  else failwith "name does not belong to player or enemy"

let get_power (ba : battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_power
  else if name = (my_list_taili (get_names ba.characters)) then ba.player_power
  else failwith "name does not belong to player or enemy"

let get_speed (ba : battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_speed
  else if name = (my_list_taili (get_names ba.characters)) then ba.player_speed
  else failwith "name does not belong to player or enemy"

let get_evasiveness (ba : battle) name = 
  if name = (my_list_hd (get_names ba.characters)) then ba.player_evasiveness
  else if name = (my_list_taili (get_names ba.characters)) 
  then ba.player_evasiveness
  else failwith "name does not belong to player or enemy"

let get_p_move_by_id (ba : battle) name id : move =
  let rec helper move_list id = 
    match move_list with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in
  if name = (my_list_hd (get_names ba.characters)) then 
    helper ba.player_moves id
  else helper ba.enemy_moves id

let get_current_pp ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (get_p_move_by_id ba name move_id).pp
  else
    failwith "name does not belong to player or enemy"

let get_move_element ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (get_p_move_by_id ba name move_id).m_element
  else
    failwith "name does not belong to player or enemy"

let set_new_health ba user_name (user_move_id : int) : float =
  if user_name = (my_list_hd (get_names ba.characters)) then
    let enemy_name = my_list_taili (get_names ba.characters) in 
    let user_move = get_move_by_id ba.characters user_name user_move_id in
    let cur_enemy_health = get_current_health ba enemy_name in
    let move_element = get_move_element ba user_name user_move_id in
    if move_element = Normal 
    then cur_enemy_health -. user_move.damage
    else if move_element = ba.enemy_element 
    then cur_enemy_health -. (0.75 *. user_move.damage)
    else if move_element = Avatar
    then cur_enemy_health -. (1.1 *. user_move.damage)
    else if move_element = Fire && ba.enemy_element = Air 
    then cur_enemy_health -. (1.25 *. user_move.damage)
    else if move_element = Earth && ba.enemy_element = Water 
    then cur_enemy_health -. (1.25 *. user_move.damage)
    else if move_element = Water && ba.enemy_element = Fire 
    then cur_enemy_health -. (1.25 *. user_move.damage)
    else if move_element = Air && ba.enemy_element = Earth 
    then cur_enemy_health -. (1.25 *. user_move.damage)
    else cur_enemy_health -. user_move.damage
  else if user_name = (my_list_taili (get_names ba.characters)) then 
    let player_name = my_list_hd (get_names ba.characters) in 
    let enemy_move = get_move_by_id ba.characters player_name user_move_id in
    let cur_player_health = get_current_health ba player_name in
    cur_player_health -. (1.0 *. enemy_move.damage)

  else failwith "name does not belong to player or enemy"

let set_new_pp ba name move_id =
  if List.mem name (get_names ba.characters)
  then
    (get_current_pp ba name move_id) - 1
  else
    failwith "name does not belong to player or enemy"

type result = Legal of t | IllegalInvalidMove | IllegalNoPP | IllegalStat

let change_pp ba name move_id : move list= 
  let filtered_move_list = 
    if name = (my_list_hd (get_names ba.characters)) then 
      List.filter (fun m -> m.id <> move_id) ba.player_moves 
    else List.filter (fun m -> m.id <> move_id) ba.enemy_moves 
  in
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
      player_moves = change_pp ba name move_id;
      enemy_moves = ba.enemy_moves;
      player_power = ba.player_power;
      player_speed = ba.player_speed;
      player_evasiveness = ba.player_evasiveness;
      enemy_element = ba.enemy_element
    }
  else
    Legal {
      characters = ba.characters;
      player_health = set_new_health ba name move_id;
      enemy_health = ba.enemy_health;
      player_moves = ba.player_moves;
      enemy_moves = change_pp ba name move_id;
      player_power = ba.player_power;
      player_speed = ba.player_speed;
      player_evasiveness = ba.player_evasiveness;
      enemy_element = ba.enemy_element
    }

let update_moves battle name old_move_id new_move_id =
  let filtered_move_list = 
    List.filter (fun move -> move.id <> old_move_id) battle.player_moves in
  let new_move = List.hd (get_new_moves battle.characters) in
  let new_move_record = {
    id = new_move.id;
    is_super = new_move.is_super;
    m_name = new_move.m_name;
    m_element = new_move.m_element;
    m_description = new_move.m_description;
    damage = new_move.damage;
    pp = new_move.pp;
  }
  in new_move_record :: filtered_move_list

let update_stats battle name (stat : string) (mult : float) (s : t2 option) =
  let stats = get_stats battle.characters name in
  let stat = String.lowercase_ascii stat in
  match stat with
  | "health" -> {stats with health = stats.health *. mult }
  | "power" -> {stats with power = stats.power *. mult }
  | "speed" -> {stats with speed = stats.speed *. mult }
  | "evasiveness" -> {stats with evasiveness = stats.evasiveness *. mult }
  | _ -> failwith "Invalid stat"

let battle_end_only_stats ba name stat mult s =
  let check_valid_stat (stat : string) =
    match stat with
    | "health" -> true
    | "power" -> true
    | "speed" -> true
    | "evasiveness" -> true
    | _ -> false
  in
  if check_valid_stat stat = false then
    IllegalStat
  else
  let new_stats = update_stats ba name stat mult s in
  Legal {
      characters = ba.characters;
      player_health = new_stats.health;
      enemy_health = ba.enemy_health;
      player_moves = ba.player_moves;
      enemy_moves = ba.enemy_moves;
      player_power = new_stats.power;
      player_speed = new_stats.speed;
      player_evasiveness = new_stats.evasiveness;
      enemy_element = ba.enemy_element
    }

let battle_end ba name old_move_id new_move_id stat mult 
    (s:Characters.t2 option): result =
  let check_valid_old_move (ba:battle) old_move_id : bool =
    let rec helper move_list old_move_id = 
      match move_list with 
      | m :: t -> if m.id = old_move_id then true else helper t old_move_id
      | _ -> false
    in
    helper ba.player_moves old_move_id
  in
  let check_valid_stat (stat : string) =
    match stat with
    | "health" -> true
    | "power" -> true
    | "speed" -> true
    | "evasiveness" -> true
    | _ -> false
  in
  if check_valid_old_move ba old_move_id = false then
    IllegalInvalidMove
  else if check_valid_stat stat = false then
    IllegalStat
  else
    let new_stats = update_stats ba name stat mult s in
    let new_moves = update_moves ba name old_move_id new_move_id in
    Legal {
      characters = ba.characters;
      player_health = new_stats.health;
      enemy_health = ba.enemy_health;
      player_moves = new_moves;
      enemy_moves = ba.enemy_moves;
      player_power = new_stats.power;
      player_speed = new_stats.speed;
      player_evasiveness = new_stats.evasiveness;
      enemy_element = ba.enemy_element
    }

let get_player_moves ba =
  ba.player_moves

let get_enemy_moves ba =
  ba.enemy_moves