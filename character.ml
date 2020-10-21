open Yojson.Basic.Util
type name = string
type four_type = Fire | Earth | Water | Air

type move = {
  mname : name;
  mtype : four_type;

}

type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

type character = {
  cname : name;
  description : string;
  isplayer: bool;
  ctype : four_type;
  stats : stats;
  moves : move list
}

type t = {
  characters : character list;
}
(*)
let character_of_json j = {
  cname = j |> member "name" |> to_string;
  description = j |> member "description" |> to_string;
  isplayer = j |> member "isplayer" |> to_bool;

}

let from_json file_name = 
  json = Yojson.Basic.from_file (file_name ^ ".json")
  {
    characters = json |> member "characters" |> to_list |> List.map character_of_json;
  }*)