(** Representation of an episode in the single-player mode*)

type battle

type t

val from_json : string -> t

val set_current_battle : t -> int -> unit

val get_characters : t -> Characters.t

val get_characters_from_save : t -> string -> Characters.t

val intro : t -> string

val enemy_line: t -> int -> string

val player_dialogue: t -> string array

val outro : t-> bool -> string

val current_battle: t -> battle

val next_battle: t -> t

val next_episode: t -> string

val move_to_next_episode: t -> bool




