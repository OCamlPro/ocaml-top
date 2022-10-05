(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

let version = "1.2.0"

type os = Linux | OSX | Windows | Other

let os = match Sys.os_type with
  | "Win32" | "Cygwin" -> Windows
  | "Unix" ->
      (try
         let in_channel = Unix.open_process_in "uname -s" in
         let os =
           try
             match input_line in_channel with
             | "Linux" -> Linux
             | "Darwin" -> OSX
             | _ -> Other
           with End_of_file -> Other
         in
         ignore (Unix.close_process_in in_channel);
         os
       with Unix.Unix_error _ -> Other)
  | _ -> Other

let is_unix = match os with
  | Linux | OSX | Other -> true
  | Windows -> false

let ( // ) = Filename.concat

let program_dir =
  let dir = Filename.dirname Sys.executable_name in
  if Filename.is_relative dir then Sys.getcwd () // dir
  else dir

let datadir =
  let default =
    if is_unix && Filename.basename program_dir = "bin" then
      Filename.dirname program_dir // "share" // "ocaml-top"
    else
      program_dir // "data"
  in
  let default =
    if Sys.file_exists default && Sys.is_directory default then default
    else Sys.getcwd () // "data"
  in
  ref default

let font =
  (* Warning, wrong font selection on Windows makes empty lines disappear ! *)
  ref (match os with
      | Linux | Other -> "DejaVu Sans Mono 10"
      | OSX -> "Monaco 12"
      | Windows -> "Lucida Console 10")

let char_width = ref 8 (* Computed once the text view is initialized *)

let theme = ref "dark"

let ocaml_cmd, ocaml_opts, stdlib_dir =
  if Sys.os_type <> "Win32"
  then ref "ocaml", ref [], None
  else if Sys.command "ocamlrun ocaml -vnum" = 0
  then ref "ocamlrun", ref ["ocaml"], None
  else
  ref (program_dir // "bin" // "ocamlrun"),
  ref [program_dir // "bin" // "ocaml"; "-I"; program_dir // "lib" // "ocaml"],
  Some (program_dir // "lib" // "ocaml")
