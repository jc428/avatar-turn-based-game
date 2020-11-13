open Yojson.Basic.Util
type element = Fire | Earth | Water | Air | Avatar | Bruh

type move = {
  id: int;
  is_super: bool;
  m_name : string;
  m_element : element;
  m_description: string;
  damage: float;
  pp: int
}

exception UnknownMove of int

type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

type t = {
  stats : stats;
  moves : move list
}

let to_element = function
  | "Air" -> Air 
  | "Fire" -> Fire 
  | "Earth" -> Earth
  | "Water" -> Water 
  | "Avatar" -> Avatar
  | _ -> Bruh

let stats_of_json j = {
  health = j |> member "health" |> to_float;
  power = j |> member "power" |> to_float;
  speed = j |> member "speed" |> to_float;
  evasiveness = j |> member "evasiveness" |> to_float;
}


let move_of_json j = {
  id = j |> member "id" |> to_int;
  m_name = j |> member "name" |> to_string;
  is_super = j |> member "issuper" |> to_bool;
  m_element = j |> member "element" |> to_string |> to_element;
  m_description = j |> member "description" |> to_string;
  damage = j |> member "damage" |> to_float;
  pp = j |> member "pp" |> to_int;
}

let from_json file_name = 
  let json = Yojson.Basic.from_file (file_name ^ ".json") in
  {
    stats = json |> member "stats" |> stats_of_json;
    moves = json |> member "moves" |> to_list |> List.map move_of_json
  }

let get_stats s = 
  s.stats

let get_moves s = 
  s.moves

let get_move_by_id s id : move =
  let rec helper move_list id = 
    match move_list with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in
  helper (get_moves s) id