type name = string
type id = int
type element = Fire | Earth | Water | Air | Normal | Avatar 

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
  c_description = "small, bald, the Avatar";
  c_element = Air;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Air Blast";
      m_element = Air;
      m_description = "Aang shoots a powerful blast of air from his staff";
      damage = 20.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Tornado Whirl";
       m_element = Air;
       m_description = "Aang spins a dazzling whirlwind at his opponent";
       damage = 10.0;
       pp = 20
     };
     {
       id = 3; is_super = false; m_name = "Spit Ball";
       m_element = Normal;
       m_description = "Aang spits on his opponent";
       damage = 5.0;
       pp = 30
     };
     {
       id = 4; is_super = false; m_name = "Dolphin Spray";
       m_element = Water;
       m_description = "Aang showers his opponent with a strong burst of water";
       damage = 12.0;
       pp = 20
     }
    ]
}

let zuko = { 
  c_name = "Zuko";
  c_description = "scarface, going through several stages of teen development, \
                   overall great guy";
  c_element = Fire;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Fire Hands";
      m_element = Fire;
      m_description = "Zuko attacks with burning red hands through strikes and \
                       punches";
      damage = 15.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Hot Head";
       m_element = Normal;
       m_description = "Zuko loses his cool and steam shoots from his ears and \
                        heats the atmosphere";
       damage = 5.0;
       pp = 30
     };
     {
       id = 3; is_super = false; m_name = "Volcano Blast";
       m_element = Earth;
       m_description = "Zuko creates a mini volcano that erupts right in front \
                        of his opponent";
       damage = 29.0;
       pp = 10
     };
     {
       id = 4; is_super = false; m_name = "Teen Angst";
       m_element = Fire;
       m_description = "Zuko chases his opponent with a series of \
                        combinations, fire and martial arts";
       damage = 21.0;
       pp = 15
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
       m_element = Normal;
       m_description =  "Ty Lee communicates with animals and has them do her \
                         bidding";
       damage = 10.0;
       pp = 25
     };
     {
       id = 3; is_super = false; m_name = "Flaming Cartwheel";
       m_element = Fire;
       m_description =  "Ty Lee surrounds her opponent with circles of flame";
       damage = 20.0;
       pp = 12
     };
     {
       id = 4; is_super = false; m_name = "Fire Flips";
       m_element = Fire;
       m_description =  "Ty Lee moves swiftly around her opponent, spreading \
                         flames across the battle ground";
       damage = 25.0;
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
       id = 2; is_super = false; m_name = "Healing Water";
       m_element = Water;
       m_description =  "Katara uses waterbending to heal a player";
       damage = -10.0;
       pp = 15
     };
     {
       id = 3; is_super = false; m_name = "Hot Mist";
       m_element = Water;
       m_description =  "Katara fills her opponent's atmosophere with scalding \
                         hot mist";
       damage = 22.0;
       pp = 10
     };
     {
       id = 4; is_super = true; m_name = "Bloodbending";
       m_element = Water;
       m_description =  "Katara manipulates her opponent's blood to control \
                         them";
       damage = 30.0;
       pp = 2
     }
    ]
}

let sokka = { 
  c_name = "Sokka";
  c_description = "dad jokes, kinda clumsy, insists he's a man to be feared";
  c_element = Water;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Blubber";
      m_element = Water;
      m_description = "Sokka throws whale blubber at his opponent's face";
      damage = 17.0;
      pp = 25
    };
     {
       id = 2; is_super = false; m_name = "Wavepool";
       m_element = Water;
       m_description =  "Sokka pushes a wavepool at his opponent";
       damage = 18.0;
       pp = 20
     };
     {
       id = 3; is_super = false; m_name = "Dad Joke";
       m_element = Normal;
       m_description =  "Sokka tells a joke so bad that his opponent loses \
                         health";
       damage = 5.0;
       pp = 25
     };
     {
       id = 4; is_super = false; m_name = "Water Martial Arts";
       m_element = Water;
       m_description =  "Sokka shows off his dank moves";
       damage = 10.0;
       pp = 15
     }
    ]
}

let toph = { 
  c_name = "Toph";
  c_description = "small, blind, sees with her feet";
  c_element = Earth;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Avalanche";
      m_element = Earth;
      m_description = "Toph creates an avalanche, crushing her opponent with \
                       large rocks";
      damage = 25.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Little Rich Girls";
       m_element = Normal;
       m_description =  "Toph uses her wealthy background to hire a random guy \
                         to punch her opponent in the face";
       damage = 15.0;
       pp = 20
     };
     {
       id = 3; is_super = false; m_name = "Blind Healing";
       m_element = Earth;
       m_description =  "Toph heals a teammate with her blind senses";
       damage = -13.0;
       pp = 12
     };
     {
       id = 4; is_super = false; m_name = "Geyser";
       m_element = Water;
       m_description =  "Toph creates a hole in the ground and a geyser erupts \
                         in her opponent's face";
       damage = 22.0;
       pp = 13
     }
    ]
}

let suki = { 
  c_name = "Suki";
  c_description = "Kyoshi warrior, proud feminist, will destroy you while \
                   calling you a sissy";
  c_element = Earth;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Fan Dance";
      m_element = Earth;
      m_description = "Suki performs a combative dance with her Kyoshi Warrior \
                       fan";
      damage = 18.0;
      pp = 15
    };
     {
       id = 2; is_super = false; m_name = "Makeup Blast";
       m_element = Normal;
       m_description =  "Suki paints her opponent's face like a Kyoshi \
                         Warrior's, making them embarrassed";
       damage = 6.0;
       pp = 25
     };
     {
       id = 3; is_super = false; m_name = "Paper Cranes";
       m_element = Air;
       m_description =  "Suki uses her fans to create paper cranes that attack \
                         her opponent";
       damage = 20.0;
       pp = 15
     };
     {
       id = 4; is_super = false; m_name = "Close Combat";
       m_element = Normal;
       m_description =  "Suki engages in close combat and throws a series of \
                         punches and kicks";
       damage = 28.0;
       pp = 10
     }
    ]
}

let mai = { 
  c_name = "Mai";
  c_description = "stoic, indifferent, in love with Zuko like the rest of us \
                   are";
  c_element = Fire;
  stats = {health = 100.0; power = 1.0; speed = 1.0; evasiveness = 1.0};
  moves = [{
      id = 1; is_super = false; m_name = "Flaming Dagger";
      m_element = Fire;
      m_description = "Mai engages in close combat with a small firey dagger";
      damage = 15.0;
      pp = 10
    };
     {
       id = 2; is_super = false; m_name = "Piercing Glare";
       m_element = Air;
       m_description =  "Mai focuses all of her supressed rage into her gaze \
                         and shoots out a concentrated beam of light that \
                         burns the skin";
       damage = 22.0;
       pp = 7
     };
     {
       id = 3; is_super = false; m_name = "Scoff";
       m_element = Normal;
       m_description =  "Mai displays an air of judgement, harming her \
                         opponent's resolve";
       damage = 5.0;
       pp = 20
     };
     {
       id = 4; is_super = false; m_name = "Hot Hair Pins";
       m_element = Fire;
       m_description =  "Mai attacks her opponent with burning hot hair pins";
       damage = 15.0;
       pp = 17
     }
    ]
}



let characters = [(1,aang); (2,zuko); (3,ty_lee); (4,katara); (5,sokka); 
                  (6,toph); (7,suki); (8,mai)]

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

let string_of_element = function
  | Fire -> "Fire"
  | Earth -> "Earth"
  | Water -> "Water"
  | Air -> "Air"
  | Normal -> "Normal"
  | Avatar -> "Avatar"

let remove_character name (lst: t list) =
  List.filter (fun (i, c) -> c.c_name <> name) lst
