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

(** Handle running an ocaml top-level as a sub-process and talking with
    it. Doesn't depend on any third-party library, portable to win32 *)

(** A handle to a running toplevel *)
type t

(** The status of a toplevel process *)
type status =
  | Starting
  | Ready
  | Busy of string
  | Dead

val add_status_change_hook : t -> (status -> unit) -> unit

type response =
  | Message of string (** Response message from ocaml *)
  | User of string (** User output from his ocaml program *)
  | Exited (** The toplevel exited or was terminated *)

(** Start a toplevel process.
    @param schedule will be run from a secondary thread and should schedule
           a function for execution in the main thread at a later point.
    @param response_handler is a callback that will be called on  output
           from ocaml.
    @param status_change_hook will be called whenever the status changes
*)
val start :
  ((unit -> unit) -> unit) ->
  (response -> unit) ->
  (status -> unit) ->
  t

(* (\** Returns [true] iff the ocaml process is running *\) *)
(* val alive : t -> bool *)

(** Send a query to a given ocaml process

    [query top code cont] passes [code] to the ocaml process and calls [cont] on
    the full response once completed. [cont] is not called in case the toplevel
    terminated.
    [code] should be a single query and not contain ';;'
*)
val query : t -> string -> (string -> unit) -> unit

(** Send an interrupt to the given ocaml process. Not working yet on windows *)
val stop : t -> unit

(** Terminates the ocaml process. The status change hook will be called *)
val kill : t -> unit
