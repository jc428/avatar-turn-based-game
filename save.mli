(**  Module that parses and writes to the save file. *)

(** [write btl character] writes battle [btl] and character [character]
    into a JSON*)
val write : Battle.battle -> Characters.t -> unit