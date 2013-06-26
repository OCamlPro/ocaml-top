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

type top = private {
  buffer: GSourceView2.source_buffer;
  mutable process: Top.t option;
  stdout_mark: GText.mark;
  ocaml_mark: GText.mark;
  prompt_mark: GText.mark;
}

val create_buffer: unit -> top

val top_start:
  init:(unit -> 'a) ->
  status_change_hook:(Top.status -> 'b) ->
  top
  -> unit

val topeval: ?full:bool -> OcamlBuffer.t -> top -> unit
