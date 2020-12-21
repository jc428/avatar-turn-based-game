open Yojson.Basic.Util
type name = string
type element = Fire | Earth | Water | Air | Normal | Avatar | Bruh
exception UnknownCharacter of name

type description = string

type move = {
  id: int;
  is_super: bool;
  m_name : name;
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

type character = {
  c_name : name;
  c_description : string;
  isplayer: bool;
  c_element : element;
  stats : stats;
  moves : move list
}

type t = {
  characters : character list;
  new_moves : move list;
  json_name : string
}

let to_element = function
  | "Air" -> Air 
  | "Fire" -> Fire 
  | "Earth" -> Earth
  | "Water" -> Water 
  | "Normal" -> Normal
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

let character_of_json j = {
  c_name = j |> member "name" |> to_string;
  c_description = j |> member "description" |> to_string;
  isplayer = j |> member "isplayer" |> to_bool;
  c_element = j |> member "element" |> to_string |> to_element;
  stats = j |> member "stats" |> stats_of_json;
  moves = j |> member "moves" |> to_list |> List.map move_of_json
}

let from_json file_name = 
  let json = Yojson.Basic.from_file (file_name ^ ".json") in
  {
    characters = json 
                 |> member "characters" 
                 |> to_list 
                 |> List.map character_of_json;
    new_moves = json 
                |> member "new moves" 
                |> to_list 
                |> List.map move_of_json;
    json_name = file_name
  }

let get_names (ch : t) : name list =
  let rec helper (c_list : character list) acc = 
    match c_list with 
    | c :: t -> helper t (c.c_name :: acc)
    | _ -> acc
  in
  List.rev (helper ch.characters [])

(* helper for getting a character record *)
let rec get_c_by_name c_list name = 
  match c_list with 
  | c :: t -> if c.c_name = name then c else get_c_by_name t name
  | [] -> raise (UnknownCharacter name)

let get_c_element ch name = 
  let character = get_c_by_name ch.characters name in
  character.c_element

let get_stats ch name = 
  let character = get_c_by_name ch.characters name in
  character.stats

let get_c_description ch name = 
  let character = get_c_by_name ch.characters name in
  character.c_description

let get_moves ch name =
  let character = get_c_by_name ch.characters name in
  character.moves

let get_new_moves ch =
  ch.new_moves

let get_move_by_id ch name id : move =
  let rec helper move_list id = 
    match move_list with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in
  helper (get_moves ch name) id

let get_move_description ch name id : description =
  (get_move_by_id ch name id).m_description

let string_of_element = function
  | Fire -> "Fire"
  | Earth -> "Earth"
  | Water -> "Water"
  | Air -> "Air"
  | Normal -> "Normal"
  | Avatar -> "Avatar"
  | Bruh -> "impossible"

type t2 = {
  stats : stats;
  moves : move list
}

let from_json_save file_name : t2 = 
  let json = Yojson.Basic.from_file (file_name ^ ".json") in
  {
    stats = json |> member "stats" |> stats_of_json;
    moves = json |> member "moves" |> to_list |> List.map move_of_json
  }

let get_stats_save s = 
  s.stats

let get_moves_save s = 
  s.moves

let get_move_by_id_save s id : move =
  let rec helper move_list id = 
    match move_list with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in
  helper (get_moves_save s) id

let character_of_json_save ch (s:t2) j = 
  let is_player = j |> member "isplayer" |> to_bool in
  let name = j |> member "name" |> to_string in
  if is_player then {
    c_name = name;
    c_description = j |> member "description" |> to_string;
    isplayer = j |> member "isplayer" |> to_bool;
    c_element = j |> member "element" |> to_string |> to_element;
    stats = get_stats_save s;
    moves = get_moves_save s
  }
  else
    {
      c_name = name;
      c_description = j |> member "description" |> to_string;
      isplayer = j |> member "isplayer" |> to_bool;
      c_element = j |> member "element" |> to_string |> to_element;
      stats = j |> member "stats" |> stats_of_json;
      moves = get_moves ch name;
    }

let characters_from_save (ch:t) (s:t2) =
  let json = Yojson.Basic.from_file (ch.json_name ^ ".json") in
  {
    characters = json 
                 |> member "characters" 
                 |> to_list 
                 |> List.map (character_of_json_save ch s);
    new_moves = json 
                |> member "new moves" 
                |> to_list 
                |> List.map move_of_json;
    json_name = ch.json_name
  }
