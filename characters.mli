(** 
   Representation of static character data.

   This module represents the data stored in episode files, including
   the characters, moves and other relevant info. It handles loading of that 
   data from JSON, querying the data, and loading a specific JSON file.
*)

(* The abstract type of valyes representing an episode *)
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

(* all characters at most have 4 moves**)
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

(*[get_names] also guarantees that the first index of the returned name list is 
  always the player character e.g. Aang*)
val get_names : t -> name list

(* [get_element] is the type of character,*)
val get_c_element : t -> name -> element

(* [get_stats] are the stats for character. *)
val get_stats : t -> name -> stats

(* [get_description] is the description of character *)
val get_c_description : t -> name -> description

(* [get_moves] returns a list representing the moveset of character. *)
val get_moves : t -> name -> move list

(* [get_move_by_id] returns a record representing a move from a move id. *)
val get_move_by_id : t -> string -> int -> move