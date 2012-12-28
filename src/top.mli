(** Handle running an ocaml top-level as a sub-process and talking with
    it. Doesn't depend on any third-party library, portable to win32 *)

(** A handle to a running toplevel *)
type t

(** Start a toplevel process.
    @param [schedule] will be run from a secondary thread and should schedule
           a function for execution in the main thread at a later point.
    @param [response_handler] is a callback that will be called on the output
           from the ocaml process.
*)
val start : ((unit -> unit) -> 'a) -> (string -> unit) -> t

(** Send a query to a given ocaml process *)
val query : t -> string -> unit

(** Send an interrupt to the given ocaml process. Not working yet on windows *)
val stop : t -> unit
