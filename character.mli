(** 
   Representation of static character data.

   This module represents the data stored in episode files, including
   the characters, moves and other relevant info. It handles loading of that 
   data from JSON, querying the data, and loading a specific JSON file.
*)

(* The abstract type of valyes representing an episode *)
type t

type name = string

type stats

type element = Fire | Earth | Water | Air | Avatar | Bruh

type description = string

type moves

(* [from_json] takes input of the JSON file name to load (DO NOT INCLUDE 
'.json') and returns an instance of t*)
val from_json : string -> t

(*[get_names a] also guarantees that the first index of the returned name list is 
always the player character e.g. Aang*)
val get_names : t -> name list

(* [get_element a] is the type of character,*)
val get_element : t -> name -> element

(* [get_stats a] are the stats for character. *)
val get_stats : t -> name -> stats

(* [get_description a] is the description of character *)
val get_description : t -> name -> description

(* [get_moves a] returns a record representing the moveset of character.  (t)*)
val get_moves : t -> name -> moves


