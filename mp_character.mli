(** Representation of static character data. *)

(** Type of a multiplay character *)
type t

(** Type of character *)
type character

(** Type of name *)
type name = string

(** Type of id *)
type id = int

(** Type of description *)
type description = string

(** UnknownCharacter is thrown when attempting to call on unknown character *)
exception UnknownCharacter of name

(** Exeption thrown when no character of [id] is available in [characters]*)
exception UnknownId of id

(** UnknownMove is thrown when attempting to call on unknown move *)
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
type element = Fire | Earth | Water | Air | Normal | Avatar

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

(** [is_character name] returns true if [name] is the name of a character in
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

(** [c_element name] returns the element of a charcter with [name] *) 
val c_element : name -> element

(** [c_stats name] returns the stats of a character with [name] *)
val c_stats : name -> stats

(** [c_description name] is the description of character with [name]*)
val c_description : name -> description

(** [c_moves name] returns a list representing the moveset of character with
    [name]. *)
val c_moves : name -> move list

(** [c_move_by_id name id] returns a record representing a character [name]'s
    move from a move id [id]. *)
val c_move_by_id : name -> int -> move

(** [c_move_description name id] returns a description of a character [name]'s
    move from a move id [id]. *)
val c_move_description : name -> int -> description

(** [string_of_element el] returns the string representation of an element [el] 
*)
val string_of_element : element -> string

(** [remove_character name lst] removes the character with name [name]
    from [lst] 
    [lst] is unchanged if no character with [name] is in [lst] *)
val remove_character : name -> t list -> t list

