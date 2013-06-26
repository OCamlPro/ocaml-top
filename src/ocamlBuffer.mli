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

type reindent_needed

type t = private {
  mutable filename: string option;
  gbuffer: GSourceView2.source_buffer;
  mutable need_reindent: reindent_needed;
  eval_mark: GSourceView2.source_mark;
  eval_mark_end: GSourceView2.source_mark;
  (* ordered from bottom to top *)
  mutable block_marks: GSourceView2.source_mark list;
  mutable on_reindent: unit -> unit;
}

val create: ?name:string -> ?contents:string -> unit -> t
val setup_indent: t -> unit

val is_modified: t -> bool
val unmodify: t -> unit
val filename: t -> string option
val filename_default: ?default:string -> t -> string
val set_filename: t -> string -> unit

val reindent_line: int -> reindent_needed
val reindent_after: int -> reindent_needed
val reindent_full: reindent_needed
val trigger_reindent: ?cont:(unit -> unit) -> t -> reindent_needed -> unit

val skip_space_backwards: t -> ?limit:GText.iter -> GText.iter -> GText.iter
val skip_space_forwards: t -> ?limit:GText.iter -> GText.iter -> GText.iter

(* These use information from the indenter, make sure it is up-to-date *)
val next_beg_of_phrase: t -> GText.iter -> GText.iter
val last_beg_of_phrase: t -> ?default:(t -> GText.iter) -> GText.iter -> GText.iter
val first_beg_of_phrase: t -> GText.iter

val get_indented_text: start:GText.iter -> stop:GText.iter -> t -> string
val contents: t -> string

val get_selection: t -> string option

module Tags: sig
  (* Tags for the toplevel *)
  val phrase: GText.tag
  val stdout: GText.tag
  val ocamltop: GText.tag
  val ocamltop_err: GText.tag
  val ocamltop_warn: GText.tag
  val invisible: GText.tag
  (* Tags for the source buffer *)
  val error: GText.tag

  val table: GText.tag_table
end

module GSourceView_params: sig
  val syntax: GSourceView2.source_language option
  val style: GSourceView2.source_style_scheme option
end
