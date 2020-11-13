(** Representation of an episode in the single-player mode*)

type battle
type t

val from_json : string -> t

val set_current_battle : t -> int -> unit

val intro : t -> string

val enemy_line: t -> int -> string

val player_dialogue: t -> string array

val outro : t-> bool -> string

val current_battle: t -> battle

val move_to_next_battle: t -> bool

val episode_done : t -> bool 




