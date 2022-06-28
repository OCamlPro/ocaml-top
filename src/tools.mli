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

val debug_enabled: bool

val debug: ('a, out_channel, unit) format -> 'a

val printexc: exn -> string

exception Recoverable_error of string

val recover_error: ('a, unit, string, 'b) format4 -> 'a

(** [string_split_chars chars str] cuts [str] at all occurence of any char
    belonging to [chars]. Empty strings are discarded. *)
val string_split_chars: string -> string -> string list

(** Splits a string into lines. Newline characters are discarded. Windows
    compatible. If the string ends with a newline, the result will end with
    an empty string (so that no information is lost) *)
val split_lines: string -> string list

module File: sig
  val load: string -> (string -> 'a) -> 'a

  (** [save contents filename k] creates a file [filename] and writes [contents] in it,
      then calling k *)
  val save: string -> string -> (unit -> unit) -> unit
end
