(** 
   Representation of save file data.

   This module represents the data stored in the save file, including
   the player's moves and other relevant info. It handles loading of that 
   data from JSON, querying the data, and loading a specific JSON file.
*)

(* The abstract type of valyes representing a save *)
type t

type name = string

type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

type element = Fire | Earth | Water | Air | Avatar | Bruh

type description = string

(* all characters have 4 moves**)
type move = {
  id: int;
  is_super: bool;
  m_name : name;
  m_element : element;
  m_description: string;
  damage: float;
  pp: int
}

(* UnknownCharacter is thrown when attempting to call on unknown character *)
exception UnknownCharacter of name

(* UnknownMove is thrown when attempting to call on unknown move *)
exception UnknownMove of int

(* [from_json] takes input of the JSON file name to load (DO NOT INCLUDE 
   '.json') and returns an instance of t*)
val from_json : string -> t

(* [get_stats] are the stats for the player. *)
val get_stats : t -> name -> stats

(* [get_moves] returns a list representing the moveset of character. *)
val get_moves : t -> name -> move list

(* [get_move_by_id] returns a record representing a move from a move id. *)
val get_move_by_id : t -> name -> int -> move