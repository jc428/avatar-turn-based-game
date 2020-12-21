(** 
   Representation of dynamic battle.
*)

(** a record representing all the dynamic state data needed for a battle *)
type battle

(** creates the initial battle state from a character record*)
val init_battle : Characters.t -> battle

(** given a character's name; returns that character's health*)
val get_current_health : battle -> Characters.name -> float

(** given a character's name; returns that character's power stat*)
val get_power : battle -> Characters.name -> float

(** given a character's name; returns that character's speed stat*)
val get_speed : battle -> Characters.name -> float

(** given a character's name; returns that character's evasiveness stat*)
val get_evasiveness: battle -> Characters.name -> float

(** given a character's name and move id; returns that move's pp stat *)
val get_current_pp : battle -> Characters.name -> int -> int

(** given a character's name and new health value, updates that character's
    health data for the battle *)
val set_new_health : battle -> Characters.name -> int -> float

(** given a character's name and move id, updates that character's
    pp for the battle *)
val set_new_pp : battle -> Characters.name -> int -> int 


(** The type representing the result of an attempted movement. *)
type result = Legal of battle | IllegalInvalidMove | IllegalNoPP | IllegalStat

(** alters the battle state given a character's name and move id, returning a 
    result *)
val make_move : battle -> Characters.name -> int -> result

(** Updates a character's moveset by replacing an old move with a new one *)
val update_moves : battle -> Characters.name -> int -> int 
  -> Characters.move list

(** Updates a character's stats by multiplying the value of a stat *)
val update_stats : battle -> Characters.name -> string -> float -> 
  Characters.t2 option -> Characters.stats

(** returns the player's move records in a list*)
val get_player_moves : battle -> Characters.move list

(** returns the opponent's move records in a list*)
val get_enemy_moves : battle -> Characters.move list

(** Given the user inputted info such as old move id, new move id, new stat to
    upgrade and a multiplier for that stat, [battle_end] returns a result variant
    containing a new state to be initialized from a save json for subsequent 
    battles. This version of the function takes in a new move update as well as
    a stats update *)
val battle_end : battle -> Characters.name -> int -> int -> string -> float 
  -> Characters.t2 option -> result

(** Given the user inputted info: new stat to upgrade and a multiplier for that
stat, [battle_end] returns a result variant containing a new state to be 
initialized from a save json for subsequent battles. This version of the 
function only takes in a stats update *)
val battle_end_only_stats : battle -> Characters.name -> string 
  -> float -> Characters.t2 option -> result