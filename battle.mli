(** 
   Representation of dynamic battle.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

type t

val init : Character.t -> t

val get_current_stats : t -> Character.name -> Character.stats

val get_current_health : t -> Character.name -> float

val get_current_pp : t -> Character.name -> int

(** The type representing the result of an attempted movement. *)
type result = Legal of t | Illegal

val make_move : string -> t -> result