(** module for the single player game play *)

(** starts the multi player game  *)
val play_sp_game : unit -> unit

(** shared function between sp and mp that uses ANSITerminal to display a 
health bar per turn *)
val make_hp_bar : int -> unit