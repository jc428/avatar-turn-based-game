open Yojson.Basic.Util
exception EndDialogue
exception NoCurrentBattle
exception InvalidBattle

type battle =
  {
    id : int;
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
  num_battles_left : int;
  episode_done: bool;
}

let battle_of_json j = 
  let enemy_dialogue =
    j |> member "enemy text" |> to_list |> List.map to_string
    |> Array.of_list 
  in
  {
    id = j |> member "id" |> to_int;
    intro = j |> member "intro" |> to_string;
    enemy_dialogue = enemy_dialogue;
    player_dialogue = j |> member "player text" |>  to_list
                      |> List.map to_string |> Array.of_list;
    outro_win = j |> member "outro_win" |> to_string;
    outro_lose = j |> member "outro_lose" |> to_string;
    move_to_next_battle = false;
  }

let from_json f_name = 
  let json_list  = Yojson.Basic.from_file f_name
                   |> member "battles" |> to_list in 
  {
    battles = json_list |> List.map battle_of_json |> Array.of_list;
    current_battle = ref 0;
    num_battles_left = List.length json_list ;
    episode_done = false;
  }

(** [i] is id of the battles*)
let set_current_battle ep i = 
  if (i <= Array.length ep.battles) then ep.current_battle := (i-1)
  else raise InvalidBattle

let current_battle ep =
  ep.battles.(!(ep.current_battle))

let intro ep = 
  (current_battle ep).intro

let player_dialogue ep =
  (current_battle ep).player_dialogue

let enemy_line ep i =
  (current_battle ep).enemy_dialogue.(i)

let outro ep win = 
  if win then (current_battle ep).outro_win
  else (current_battle ep).outro_lose

let move_to_next_battle ep =
  false

let episode_done ep = 
  ep.episode_done

