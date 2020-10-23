(** 
   Representation of dynamic battle.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

type t

val init_battle : Characters.t -> t

val get_player_health : t -> Characters.name -> float

val get_enemy_health : t -> Characters.name -> float

val get_current_pp : t -> Characters.name -> int

(** The type representing the result of an attempted movement. *)
type result = Legal of t | IllegalInvalidMove | IllegalNoPP

val make_move : string -> t -> result