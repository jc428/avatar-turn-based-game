(* writing to json file *)
open Sys
open Yojson

let episode : [> `Assoc of (string * [> `String of string ]) list ] =
`Assoc [("episode", `Int 1)]

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

