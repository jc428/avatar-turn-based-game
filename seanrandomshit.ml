(* writing to json file *)
open Sys
open Yojson

(* Declares and sets json values

[> `Assoc of (string * [> `Assoc of (string * [> `String of string ]) list ]) list ] 
*)
let episode =
`List [(`Assoc [
                  ("health", `Int 1); 
                  ("power", `Int 1)
                ]
        );
        
        (`List [
                `Assoc [
                        ("id", `Int 1);
                        ("name", `String "move1");
                        ("issuper", `Bool false);
                        ("element", `String "1");
                        ("description", `String "1");
                        ("damage", `Float 1.0);
                        ("pp", `Int 1)
                        ]; 
                `Assoc [
                        ("id", `Int 2);
                        ("name", `String "move2");
                        ("issuper", `Bool false);
                        ("element", `String "Fire");
                        ("description", `String "2");
                        ("damage", `Float 2.0);
                        ("pp", `Int 2)
                        ];
                `Assoc [
                        ("id", `Int 3);
                        ("name", `String "move3");
                        ("issuper", `Bool false);
                        ("element", `String "Water");
                        ("description", `String "3");
                        ("damage", `Float 3.0);
                        ("pp", `Int 3)
                        ];
                `Assoc [
                        ("id", `Int 4);
                        ("name", `String "move4");
                        ("issuper", `Bool false);
                        ("element", `String "Air");
                        ("description", `String "4");
                        ("damage", `Float 4.0);
                        ("pp", `Int 4)
                        ];
                  ]
        )
       ]

(* creates/overrides json save file in current directory *)
let () = 
  Yojson.Basic.to_file "save_file.json" episode

(* 

(*ascii demo*)

exception DoneWithAscii
let read filename =
  let ic = open_in filename in
  let rec process_line () =
    let line = try input_line ic with End_of_file -> raise DoneWithAscii
    in
       print_endline line;
       process_line ();
in process_line ()

let () = try read "aang-non-battle.txt" with DoneWithAscii -> () 

*)

