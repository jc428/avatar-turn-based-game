open Yojson.Basic.Util
exception EndDialogue
exception NoCurrentBattle
exception InvalidBattle

type battle =
  {
    id : int;
    character_json : string;
    intro: string;
    enemy_dialogue : string array;
    player_dialogue: string array;
    outro_win : string;
    outro_lose: string;
    move_to_next_battle: bool;
  }

type t = {
  battles : battle array;
  current_battle : int ref;
  last_battle: bool;
  next_episode: string;
}

let battle_of_json j = 
  let enemy_dialogue =
    j |> member "enemy text" |> to_list |> List.map to_string
    |> Array.of_list 
  in
  {
    id = j |> member "id" |> to_int;
    character_json = j |> member "character json" |> to_string;
    intro = j |> member "intro" |> to_string;
    enemy_dialogue = enemy_dialogue;
    player_dialogue = j |> member "player text" |>  to_list
                      |> List.map to_string |> Array.of_list;
    outro_win = j |> member "outro_win" |> to_string;
    outro_lose = j |> member "outro_lose" |> to_string;
    move_to_next_battle = false;
  }

let from_json f_name = 
  let j = Yojson.Basic.from_file f_name in
  let json_list  = j |> member "battles" |> to_list in 
  {
    battles = json_list |> List.map battle_of_json |> Array.of_list;
    current_battle = ref 0;
    last_battle = false;
    next_episode = j |> member "next episode" |> to_string;
  }

(** [i] is id of the battles*)
let set_current_battle ep i = 
  if (i <= Array.length ep.battles) then ep.current_battle := (i-1)
  else raise InvalidBattle

let current_battle ep =
  ep.battles.(!(ep.current_battle))

let get_characters ep = 
  Characters.from_json ((current_battle ep).character_json)

let get_characters_from_save ep save_name =
  let ch = get_characters ep in
  Characters.characters_from_save ch (Characters.from_json_save save_name)

let intro ep = 
  (current_battle ep).intro

let player_dialogue ep =
  (current_battle ep).player_dialogue

let enemy_line ep i =
  (current_battle ep).enemy_dialogue.(i)

let outro ep win = 
  if win then (current_battle ep).outro_win
  else (current_battle ep).outro_lose

let next_battle ep =
  {
    battles = ep.battles;
    current_battle = ref (!(ep.current_battle) + 1);
    last_battle = !(ep.current_battle) = (Array.length ep.battles - 2);
    next_episode = ep.next_episode;
  }

let next_episode ep =
  ep.next_episode

let move_to_next_episode ep =
  ep.last_battle


