(** 
   Representation of static character data.

   This module represents the data stored in battle files, including
   the characters, moves and other relevant info. It handles loading of that 
   data from JSON, querying the data, and loading a specific JSON file.
*)

(** The abstract type of values representing character data *)
type t

(** Abstract type representing a save file *)
type t2

(** type of a character's name *)
type name = string

(** type of a characters' stats *)
type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

(** type for elements *)
type element = Fire | Earth | Water | Air | Normal | Avatar | Bruh

(** type for character descriptions *)
type description = string

(** type for character moves
    all characters have at most 4 moves*)
type move = {
  id: int;
  is_super: bool;
  m_name : name;
  m_element : element;
  m_description: string;
  damage: float;
  pp: int
}

(** UnknownCharacter is thrown when attempting to call on unknown character *)
exception UnknownCharacter of name

(** UnknownMove is thrown when attempting to call on unknown move *)
exception UnknownMove of int

(** [from_json] takes input of the JSON file name to load (DO NOT INCLUDE 
    '.json') and returns an instance of t*)
val from_json : string -> t

(**[get_names] also guarantees that the first index of the returned name list is 
   always the player character e.g. Aang*)
val get_names : t -> name list

(** [get_element] is the type of character,*)
val get_c_element : t -> name -> element

(** [get_stats] are the stats for character. *)
val get_stats : t -> name -> stats

(** [get_description] is the description of character *)
val get_c_description : t -> name -> description

(** [get_moves] returns a list representing the moveset of character. *)
val get_moves : t -> name -> move list

(** [get_new_moves] returns a list representing the new moves a character can 
    learn *)
val get_new_moves : t -> move list

(** [get_move_by_id] returns a record representing a move from a move id. *)
val get_move_by_id : t -> name -> int -> move

(** [get_move_description] returns the decritpion of a move from a move id. *)
val get_move_description : t -> name -> int -> description

(** [string_of_element] returns the string representation of an element *)
val string_of_element : element -> string

(** [from_json_save] takes input of the JSON file name to load (DO NOT INCLUDE 
    '.json') and returns an instance of t*)
val from_json_save : string -> t2

(** [get_stats_save] are the stats for the player. *)
val get_stats_save : t2 -> stats

(** [get_moves_save] returns a list representing the moveset of character. *)
val get_moves_save : t2 -> move list

(** [get_move_by_id_save] returns a record representing a move from a move id. *)
val get_move_by_id_save : t2 -> int -> move

(** [characters_from_save] returns the characters from a saved JSON file*)
val characters_from_save : t -> t2 -> t
