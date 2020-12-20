(** Representation of an episode in the single-player mode*)

(** [battle] is the representation of a single player battle *)
type battle

(** type of an episode *)
type t

(** [from_json str] initalizes an episode from a json file
    with the name [str] *)
val from_json : string -> t

(** [set_current_battle ep i] sets the current battle being played as battle 
    to number [i] in episode [ep] *)
val set_current_battle : t -> int -> unit

(** [get_characters ep] returns all the characters in play for the current
    battle  *)
val get_characters : t -> Characters.t

(** [get_characters_from_save ep str] gets characters for episode [ep] 
    from the saved information in the json file with file name [str]  *)
val get_characters_from_save : t -> string -> Characters.t

(** [intro ep] is the starting dialogue for the current battle in [ep] *)
val intro : t -> string

(** [enemy_line ep i] is the enemy dialogue number [i] for the current battle 
    in [ep] *)
val enemy_line: t -> int -> string

(** [player_dialogue ep] is an array of the player dialogues for the current 
    battle in [ep] *)
val player_dialogue: t -> string array

(** [outro ep win] is the ending dialogue for [ep].
    If [win] is true it is the dialogue for when the player wins, and 
    if [win] is false it is the dialogue for when the player loses. *)
val outro : t-> bool -> string

(** [current_battle ep] is the current battle in play in episode [ep] *)
val current_battle: t -> battle

(** [next_battle ep] is the next battle in sequence in [ep] *)
val next_battle: t -> t

(** [next_episode ep] is the name of the episode json following [ep] *)
val next_episode: t -> string

(** [move_to_next_episode ep] is true if the game is at the end of the current
    episode and should move to the next one, and false otherwise*)
val move_to_next_episode: t -> bool




