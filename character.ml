open Yojson.Basic.Util
type name = string
type element = Fire | Earth | Water | Air | Avatar | Bruh

type move = {
  m_name : name;
  m_element : element;

}

type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

type character = {
  c_name : name;
  description : string;
  isplayer: bool;
  c_element : element;
  stats : stats;
  moves : move list
}

type t = {
  characters : character list;
}

let to_element = function
  | "Air" -> Air
  | "Fire" -> Fire 
  | "Earth" -> Earth
  | "Water" -> Water 
  | "Avatar" -> Avatar
  | _ -> Bruh

let stats_of_json =
  failwith "to do"

let moves_of_json =
  failwith "to do"

let character_of_json j = {
  c_name = j |> member "name" |> to_string;
  description = j |> member "description" |> to_string;
  isplayer = j |> member "isplayer" |> to_bool;
  c_element = j |> member "element" |> to_string |> to_element;
  stats = j |> member "stats" |> stats_of_json;
  moves = j |> member "moves" |> to_list |> List.map moves_of_json

}

let from_json file_name = 
  let json = Yojson.Basic.from_file (file_name ^ ".json") in
  {
    characters = json |> member "characters" |> to_list |> List.map character_of_json;
  }