#require "core"
open Core

let line_helper line =
  printf "%s \n" line

(* try read with input "aang-non-battle.txt" *)
let read file =
    let f = file in
      let lines = In_channel.read_lines f in
        List.iter ~f: line_helper lines