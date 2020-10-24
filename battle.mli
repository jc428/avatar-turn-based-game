(* 
   Representation of dynamic battle.

   This module represents the state of an adventure as it is being played,
   including the adventurer's current room, the rooms that have been visited,
   and functions that cause the state to change.
*)

type battle

val init_battle : Characters.t -> battle

val get_current_health : battle -> Characters.name -> float

val get_current_pp : battle -> Characters.name -> int -> int

val set_new_health : battle -> Characters.name -> int -> float

val set_new_pp : battle -> Characters.name -> int -> int 


(** The type representing the result of an attempted movement. *)
type result = Legal of battle | IllegalInvalidMove | IllegalNoPP

val make_move : battle -> Characters.name -> int -> result