(* 
   Representation of dynamic battle.
*)

type battle

val init_battle : Characters.t -> battle

val get_current_health : battle -> Characters.name -> float

val get_power : battle -> Characters.name -> float

val get_speed : battle -> Characters.name -> float

val get_evasiveness: battle -> Characters.name -> float

val get_current_pp : battle -> Characters.name -> int -> int

val set_new_health : battle -> Characters.name -> int -> float

val set_new_pp : battle -> Characters.name -> int -> int 


(** The type representing the result of an attempted movement. *)
type result = Legal of battle | IllegalInvalidMove | IllegalNoPP | IllegalStat

val make_move : battle -> Characters.name -> int -> result

(* Updates a character's moveset by replacing an old move with a new one *)
val update_moves : battle -> Characters.name -> int -> int 
  -> Characters.move list

(* Updates a character's stats by multiplying the value of a stat *)
val update_stats : battle -> Characters.name -> string -> float -> 
  Characters.t2 option -> Characters.stats

val get_player_moves : battle -> Characters.move list

val get_enemy_moves : battle -> Characters.move list

val battle_end : battle -> Characters.name -> int -> int -> string -> float 
  -> Characters.t2 option -> result