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
           | `EXECUTE | `EXECUTE_ALL | `STOP | `RESTART | `CLEAR
           | `SELECT_FONT | `SELECT_COLOR | `ZOOM_IN | `ZOOM_OUT | `FULLSCREEN
           | `QUIT ]
  val bind: t -> (unit -> unit) -> unit
  (* val trigger: t -> unit *)

  val enable: t -> unit
  val disable: t -> unit
end

module Dialogs : sig
  type 'a cps = ('a -> unit) -> unit

  val choose_file : [< `OPEN | `SAVE ] -> ?cancel:(unit -> unit) -> string cps

  val error : title:string -> string -> unit

  val quit : string option
    -> save:(unit -> unit cps) -> quit:(unit -> unit)
    -> unit

  val confirm : title:string -> string -> ?no:(unit -> unit) -> unit cps

  val choose_font :
    GSourceView2.source_view -> GSourceView2.source_view ->
    on_font_change:(unit -> unit) -> unit
    -> unit
end

val main_window : unit -> GWindow.window

val set_window_title :
  GWindow.window -> ('a, unit, string, string, string, unit) format6 -> 'a

val open_text_view : GSourceView2.source_buffer -> GSourceView2.source_view

val open_toplevel_view : GSourceView2.source_buffer -> GSourceView2.source_view

(* Displays a message in the status bar *)
val top_msg : string -> unit
val index_msg : string -> unit

(* call _after_ opening the views *)
val set_font :
  GSourceView2.source_view -> GSourceView2.source_view -> string
  -> unit

val switch_fullscreen : GWindow.window -> unit
