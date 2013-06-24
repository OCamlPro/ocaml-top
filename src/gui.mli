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

module Controls : sig
  type t = [ `NEW | `OPEN | `SAVE | `SAVE_AS
           | `EXECUTE | `EXECUTE_ALL | `STOP | `CLEAR | `RESTART
           | `QUIT ]

  val bind: t -> (unit -> unit) -> unit
  (* val trigger: t -> unit *)

  val enable: t -> unit
  val disable: t -> unit
end

module Dialogs : sig
  val choose_file : [< `OPEN | `SAVE ] -> (string -> unit) -> unit

  val error : title:string -> string -> unit

  val quit : string option -> (unit -> bool) -> bool

  val confirm : title:string -> string -> (unit -> unit) -> unit
end

val main_window : GWindow.window

val set_window_title : ('a, unit, string, string, string, unit) format6 -> 'a

val open_text_view : GSourceView2.source_buffer -> GSourceView2.source_view

val open_toplevel_view : GSourceView2.source_buffer -> GSourceView2.source_view
