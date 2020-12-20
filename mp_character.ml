type name = string
type id = int
type element = Fire | Earth | Water | Air | Avatar | Bruh

exception UnknownCharacter of name
exception UnknownMove of id
exception UnknownId of id

type description = string

type move = {
  id: int;
  is_super: bool;
  m_name : name;
  m_element : element;
  m_description: string;
  damage: float;
  pp: int
}

type stats = {
  health: float;
  power: float;
  speed: float;
  evasiveness: float
}

type character = 
  {
    c_name : name;
    c_description : string;
    c_element : element;
    stats : stats;
    moves : move list
  }

type t = id * character

let aang = { 
  c_name = "Aang";
  c_description = "hes da  Avatar broo0o";
  c_element = Air;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Air blast";
      m_element = Air;
      m_description = "Aang shoots a powerful blast of air from his staff";
      damage = 15.0;
      pp = 10
    };
     {
       id = 2; is_super = false; m_name = "Tornado whirl";
       m_element = Air;
       m_description = "Aang spins a dazzling whirlwind at his opponent";
       damage = 10.0;
       pp = 20
     }
    ]
}

let zuko = { 
  c_name = "Zuko";
  c_description = "DISGRACED PRINCE RAWRRR XD";
  c_element = Fire;
  stats = {health = 69.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Fire ball";
      m_element = Fire;
      m_description = "Zuko shoots a powerful blast of fire";
      damage = 15.0;
      pp = 10
    };
     {
       id = 2; is_super = false; m_name = "Fire breath";
       m_element = Fire;
       m_description = "Zuko harnesses his inner dragon, \
                        unleashing a breath of fire";
       damage = 8.0;
       pp = 25
     }
    ]
}

let ty_lee = { 
  c_name = "Ty Lee";
  c_description = "a bit of an airhead but very cheerful and pretty";
  c_element = Fire;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Acrobatics";
      m_element = Fire;
      m_description = "Ty Lee stuns and confuses her opponent with a sequence \
                       of acrobatic movements combined with bursts of fire";
      damage = 10.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Animal Whisper";
       m_element = Fire;
       m_description =  "idk why but I feel like Ty Lee could communicate with \
                         animals and have them do her bidding";
       damage = 20.0;
       pp = 10
     }
    ]
}

let katara = { 
  c_name = "Katara";
  c_description = "mom friend, pretty hair, crush on Aang";
  c_element = Water;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Splash";
      m_element = Water;
      m_description = "Katara stuns her opponent with a medium splash of \
                       water to the face";
      damage = 10.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Heal";
       m_element = Water;
       m_description =  "Katara uses waterbending to heal a teammate";
       damage = -10.0;
       pp = 15
     }
    ]
}

let characters = [(1,aang); (2,zuko); (3,ty_lee); (4,katara)]

let is_character name = 
  let rec helper assoc =
    match assoc with 
    | (i, n) :: t -> if n.c_name = name then true else helper t
    | [] -> false
  in helper characters

let names char_list = 
  let rec helper lst acc = 
    match lst with 
    | (i,n ) :: t -> helper t (n.c_name :: acc)
    | _ -> acc
  in
  List.rev (helper char_list [])

let name (character: t) =
  match character with
  | (i,c) -> c.c_name

let id character = 
  match character with 
  | (i, c) -> i

let id_to_name id = 
  match List.assoc_opt id characters with 
  | None -> raise (UnknownId id)
  | Some c -> c.c_name 

let c_by_name name = 
  let rec helper lst =
    match lst with 
    | (i, c) :: t -> if c.c_name = name then c else helper t
    | [] -> raise (UnknownCharacter name)
  in 
  helper characters

let c_by_id id = 
  let rec helper lst =
    match lst with 
    | (i, c) :: t -> if i = id then c else helper t
    | [] -> raise (UnknownId id)
  in 
  helper characters

let c_element name = 
  let character = c_by_name name in
  character.c_element

let c_stats name = 
  let character = c_by_name name in
  character.stats

let c_description name = 
  let character = c_by_name name in
  character.c_description

let c_moves name =
  let character = c_by_name name in
  character.moves

let c_move_by_id name id : move =
  let rec helper move_list id = 
    match move_list with 
    | m :: t -> if m.id = id then m else helper t id
    | _ -> raise (UnknownMove id)
  in
  helper (c_moves name) id

let c_move_description name id : description =
  (c_move_by_id name id).m_description

let remove_character name (lst: t list) =
  List.filter (fun (i, c) -> c.c_name <> name) lst
