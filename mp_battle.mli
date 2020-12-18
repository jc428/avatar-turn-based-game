(** Representation of a dynamic multiplayer t
*)
type t

type name = string

exception PlayerNotFound of name

(** [init_battle names] creates a mp_battle from a list of
    character names [names] 
    Require: name is length 4 
    Raise: UnknownCharacter name if there are no character with name [name]
    in Mp_character.characters *)
val init_battle : name list -> t

(** [players btl] returns a list of all four players in battle [btl]*)
val players : t -> name list

(** [current_health btl name] returns the current health value of the player
    with name [name] in the battle [btl] *)
val current_health : t -> name -> float

(** [power btl name] returns the power value of the player
    with name [name] in the battle [btl] *)
val power : t -> name -> float

(** [speed btl name] returns the speed value of the player
    with name [name] in the battle [btl] *)
val speed : t -> name -> float

(** [evasiveness btl name] returns the evasiveness value of the player
    with name [name] in the battle [btl] *)
val evasiveness: t -> name -> float

(**  [current_pp btl name id] returns the remaining number of times the move 
     with id [id] of the player with name [name] in the battle [btl]. *)
val current_pp : t -> name -> int -> int

(**  [new_health btl name move_id target] calculates the new health of the 
     player with name [target] as a result of player with the name [name] using 
     move with id [move_id] on the [target]. *)
val new_health : t -> name -> int -> name -> float

(** The type representing the result of an attempted move. *)
type result = Legal of t | IllegalInvalidMove | IllegalNoPP 

(** [make_move btl name id target] is the result of player [name] using 
    move [id] on player [target] in battle [btl] *)
val make_move : t -> name -> int -> name -> result

(** [player_moves btl name] is the list of moves belonging to player [name] 
    in battle [btl] *)
val player_moves : t -> name -> Mp_character.move list