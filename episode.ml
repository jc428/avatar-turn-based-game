open Yojson.Basic.Util
exception EndDialogue
exception NoCurrentBattle

type battle =
  {
    id : int;
    intro: string;
    enemy_dialogue : string list;
    enemy_dialogue_i: int;
    player_dialogue: string list;
    outro_win : string;
    outro_lose: string;
    move_to_next_battle: bool;
  }

type t = {
  battles : battle list;
  current_battle : battle option;
  num_battles_left : int;
  episode_done: bool;
}

let battle_of_json j = {
  id = j |> member "id" |> to_int;
  intro = j |> member "intro" |> to_string;
  enemy_dialogue = j |> member "enemy text" |> to_list |> List.map to_string;
  enemy_dialogue_i = 0;
  player_dialogue = j |> member "player text" |>  to_list |> List.map to_string;
  outro_win = j |> member "outro win" |> to_string;
  outro_lose = j |> member "outro lose" |> to_string;
  move_to_next_battle = false;
}

let from_json file = 
  let json_list  = Yojson.Basic.from_file file
                   |> member "battles" |> to_list in 
  {
    battles = json_list |> List.map battle_of_json;
    current_battle = None;
    num_battles_left = List.length json_list ;
    episode_done = false;
  }

let set_current_battle ep i = 
  let rec helper = function
    | [] -> failwith "No battles left"
    | h :: t -> begin
        if h.id = i then h
        else helper t
      end
  in 
  helper ep.battles

let intro ep = 
  match ep.current_battle with
  | None -> raise NoCurrentBattle
  | Some btl -> btl.intro

let player_dialogue ep =
  match ep.current_battle with
  | None -> raise NoCurrentBattle
  | Some btl -> btl.player_dialogue

let next_enemy_dialogue ep =
  ""

let outro ep win = 
  match ep.current_battle with
  | None -> raise NoCurrentBattle
  | Some btl -> if win then btl.outro_win else btl.outro_lose

let current_battle ep =
  match ep.current_battle with
  | None -> raise NoCurrentBattle
  | Some btl -> btl.id |> string_of_int

let move_to_next_battle ep =
  false

let episode_done ep = 
  ep.episode_done

