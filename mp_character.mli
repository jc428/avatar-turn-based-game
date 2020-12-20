(** Representation of static character data. *)

(** Type of a multiplay character *)
type t

(** Type of character *)
type character

type name = string

type id = int

type description = string

(** UnknownCharacter is thrown when attempting to call on unknown character *)
exception UnknownCharacter of name

(** Exeption thrown when no character of [id] is available in [characters]*)
exception UnknownId of id

(* UnknownMove is thrown when attempting to call on unknown move *)
exception UnknownMove of int

(** Type of the properties of a character *)
type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

(** Type of character and move element
    Determines effectiveness of moves*)
type element = Fire | Earth | Water | Air | Avatar | Normal | Bruh

(** Type of character moves *)
type move = {
  id: int;
  is_super: bool;
  m_name : name;
  m_element : element;
  m_description: string;
  damage: float;
  pp: int
}

(** [characters] contains all of the characters available for selection
    for the player and the opponent *)
val characters : t list

(** [id character] is the id of the character [character] *)
val id : t -> id

(** [name character] is the name of the character [character] *)
val name : t -> name

(** [is_character] returns true if [name] is the name of a character in
    [characters] and false otherwise *)
val is_character : name -> bool

(** [get_names char_list] returns a list of the names of all characters in a 
    list of characters [char_list] *)
val names : t list -> name list 

(** [id_to_name id] returns the name of the character with id [id] *)
val id_to_name : id -> name 

(** [c_by_name name] returns the character with [name]
    Raises: UnknownCharacter [name] if no such character exists *)
val c_by_name : name -> character      

(** [c_element] returns the element of a charcter *) 
val c_element : name -> element

(** [c_stats] returns the stats of a character *)
val c_stats : name -> stats

(** [c_description] is the description of character *)
val c_description : name -> description

(** [c_moves] returns a list representing the moveset of character. *)
val c_moves : name -> move list

(** [c_move_by_id] returns a record representing a move from a move id. *)
val c_move_by_id : name -> int -> move

(** [c_move_description] returns a description of a move from a move id. *)
val c_move_description : name -> int -> description

(** [remove_character name lst] removes the character with name [name]
    from [lst] 
    [lst] is unchanged if no character with [name] is in [lst] *)
val remove_character : name -> t list -> t list

