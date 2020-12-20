open Mp_character

type player = Mp_character.t
exception PlayerNotFound of name

type name = string

type team = {
  members : name * name;
  playerA_name : name;
  playerA_description : string;
  playerA_element: element;
  playerA_stats : stats;
  playerA_moves : move list;
  playerB_name : name;
  playerB_description : string;
  playerB_element: element;
  playerB_stats : stats;
  playerB_moves : move list;
}

type t = {
  team1 : team;
  team2 : team;
}

let find_character name = 
  if is_character name then name
  else raise (UnknownCharacter name)

let mp_init_battle names = 
  match names with 
  | [a; b; c; d] ->
    let player1 = find_character a in 
    let player2 = find_character b in 
    let player3 = find_character c in 
    let player4 = find_character d in 
    let init_helper pl1 pl2 = 
      {
        members = (pl1, pl2);
        playerA_name = pl1;
        playerA_description = c_description pl1;
        playerA_element = c_element pl1;
        playerA_stats = c_stats pl1;
        playerA_moves = c_moves pl1;
        playerB_name = pl2;
        playerB_description = c_description pl2;
        playerB_element = c_element pl2;
        playerB_stats = c_stats pl2;
        playerB_moves = c_moves pl2;
      } 
    in
    let team1 = init_helper player1 player2 in 
    let team2 = init_helper player3 player4 in
    {team1 = team1; team2 = team2} 
  | _ -> failwith "precondition violated"

let team1 btl = 
  btl.team1.members

let team2 btl = 
  btl.team2.members

let is_team1 btl name = 
  match btl.team1.members with
  | (a, b) -> name = a || name = b

let is_team2 btl name = 
  match btl.team2.members with
  | (a, b) -> name = a || name = b

let players btl = 
  let t1 = 
    match btl.team1.members with
    | (a, b) -> [a;b]
  in let t2 =
       match btl.team2.members with
       | (a, b) -> [a;b]
  in t1 @ t2

let player_stats btl name = 
  let helper team = 
    match team.members with 
    | (a, b) -> begin 
        if name = a then team.playerA_stats
        else if name = b then team.playerB_stats
        else raise (PlayerNotFound name)
      end
  in 
  if is_team1 btl name then 
    helper btl.team1
  else
    helper btl.team2

let current_health btl name = 
  (player_stats btl name).health

let power btl name = 
  (player_stats btl name).power

let speed btl name = 
  (player_stats btl name).speed

let evasiveness btl name = 
  (player_stats btl name).evasiveness

let player_moves btl name = 
  let helper team = 
    match team.members with 
    | (a, b) -> begin 
        if name = a then team.playerA_moves
        else if name = b then team.playerB_moves
        else raise (PlayerNotFound name)
      end
  in 
  if is_team1 btl name then 
    helper btl.team1
  else
    helper btl.team2

let move_by_id btl name id = 
  let rec helper moves id = 
    match moves with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in 
  helper (player_moves btl name) id

let current_pp btl name move_id = 
  (move_by_id btl name move_id).pp

let element_by_name btl name = 
  let helper team = 
    match team.members with 
    | (a, b) -> begin 
        if name = a then team.playerA_element
        else if name = b then team.playerB_element
        else raise (PlayerNotFound name)
      end
  in 
  if is_team1 btl name then 
    helper btl.team1
  else
    helper btl.team2

let new_health btl name move_id target = 
  let move = move_by_id btl name move_id in
  let move_element = move.m_element in
  let target_element = element_by_name btl name
  in
  if move_element = Normal 
  then current_health btl target -. move.damage
  else if move_element = Avatar || target_element = Avatar 
  then current_health btl target -. (1.1 *. move.damage)
  else if move_element = target_element 
  then current_health btl target -. (0.75 *. move.damage)
  else if move_element = Fire && target_element = Air 
  then current_health btl target -. (1.25 *. move.damage)
  else if move_element = Earth && target_element = Water 
  then current_health btl target -. (1.25 *. move.damage)
  else if move_element = Water && target_element = Fire 
  then current_health btl target -. (1.25 *. move.damage)
  else if move_element = Air && target_element = Earth 
  then current_health btl target -. (1.25 *. move.damage)
  else current_health btl target -. move.damage

(** The type representing the result of an attempted move. *)
type result = Legal of t | IllegalInvalidMove | IllegalNoPP

let update_move_list btl name id = 
  let moves = player_moves btl name in 
  let move = move_by_id btl name id in 
  let new_pp = move.pp - 1 in 
  let updated_move = 
    { 
      id = move.id;
      is_super = move.is_super;
      m_name = move.m_name;
      m_element = move.m_element;
      m_description = move.m_description;
      damage = move.damage;
      pp = new_pp
    }
  in 
  let filtered_move_list =  List.filter (fun m -> m.id <> id) moves in
  let rec ordered_move_list i move = function
    | [] -> [move]
    | h :: t -> if i = 0 then move :: (h :: t)
      else h :: (ordered_move_list (i-1) move t)
  in ordered_move_list (id-1) updated_move filtered_move_list

let update_team team btl name id target = 
  let old_stats = player_stats btl target in 
  let new_stats =  { 
    health = new_health btl name id target;
    power = old_stats.power;
    speed = old_stats.speed;
    evasiveness = old_stats.evasiveness
  } in
  match team.members with
    (a, b) -> if target = a && name = b then { (* target a name b *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = new_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = update_move_list btl name id;
    }
    else if target = b && name = a then {  (* target b name a *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = update_move_list btl name id;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = new_stats;
      playerB_moves = team.playerB_moves;
    }
    else if name = a && target = a then { (* move on self *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = new_stats; 
      playerA_moves = update_move_list btl name id;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = team.playerB_moves;
    }
    else if name = b && target = b then { (* move on self *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = new_stats;
      playerB_moves = update_move_list btl name id;
    }
    else if target = a then {         (* target a name other team *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = new_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = team.playerB_moves;
    }
    else if target = b then {         (* target b name other team *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = new_stats;
      playerB_moves = team.playerB_moves;
    }
    else if name = a then {           (* name a target other team *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = update_move_list btl name id;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = team.playerB_moves;
    }

    else if name = b then {            (* name b target other team *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = update_move_list btl name id;
    }
    else {                             (* nothing happened to this team *)
      members = team.members;
      playerA_name = team.playerA_name;
      playerA_description = team.playerA_description;
      playerA_element = team.playerA_element;
      playerA_stats = team.playerA_stats; 
      playerA_moves = team.playerA_moves;
      playerB_name = team.playerB_name;
      playerB_description = team.playerB_description;
      playerB_element = team.playerB_element;
      playerB_stats = team.playerB_stats;
      playerB_moves = team.playerB_moves;
    }

let mp_make_move btl name id target = 
  match move_by_id btl name id with 
  | move -> begin 
      Legal {
        team1 = update_team btl.team1 btl name id target;
        team2 = update_team btl.team2 btl name id target
      }
    end
  | exception UnknownMove id -> IllegalInvalidMove
