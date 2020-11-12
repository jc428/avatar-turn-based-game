(** Representation of an episode in the single-player mode*)

type battle
type t

val from_json : string -> t

val set_current_battle : t -> int -> battle

val intro : t -> string

val next_enemy_dialogue: t -> string

val player_dialogue: t -> string list

val outro : t-> bool -> string

val current_battle: t -> string

val move_to_next_battle: t -> bool

val episode_done : t -> bool 




