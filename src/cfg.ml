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
  let default =
    let dir = Filename.dirname Sys.executable_name in
    let dir =
      if Filename.is_relative dir then
        Filename.concat (Sys.getcwd ()) dir
      else dir
    in
    Filename.concat dir "data"
  in
  let default =
    if Sys.file_exists default && Sys.is_directory default then default
    else Filename.concat (Sys.getcwd ()) "data"
  in
  ref default

let font = ref "Lucida Console,DejaVu Sans Mono,Monospace Regular 10"

let char_width = ref 8

let ocamlrun_path = ref "ocamlrun"

let ocaml_path = ref "ocaml"

let ocaml_opts : string list ref = ref []
