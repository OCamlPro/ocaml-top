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

let datadir =
  let (/) = Filename.concat in
  let program_dir =
    let dir = Filename.dirname Sys.executable_name in
    if Filename.is_relative dir then Sys.getcwd () / dir
    else dir
  in
  let default =
    if Sys.os_type = "Unix" && Filename.basename program_dir = "bin" then
      Filename.dirname program_dir / "share" / "ocaml-top"
    else
      program_dir / "data"
  in
  let default =
    if Sys.file_exists default && Sys.is_directory default then default
    else Sys.getcwd () / "data"
  in
  ref default

let font =
  (* Warning, wrong font selection on Windows makes empty lines disappear ! *)
  ref (match Sys.os_type with
      | "Unix" -> "DejaVu Sans Mono 10"
      | _ -> "Lucida Console 10")

let char_width = ref 8

let ocamlrun_path = ref "ocamlrun"

let ocaml_path = ref "ocaml"

let ocaml_opts : string list ref = ref []
