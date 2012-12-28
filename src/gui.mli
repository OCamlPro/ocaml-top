module Controls : sig
  type t = [ `NEW | `OPEN | `SAVE | `SAVE_AS
           | `EXECUTE | `STOP
           | `QUIT ]

  val bind: t -> (unit -> unit) -> unit
  (* val trigger: t -> unit *)
end

module Dialogs : sig
  val choose_file : [< `OPEN | `SAVE ] -> (string -> unit) -> unit

  val error : title:string -> string -> unit

  val quit : string option -> (unit -> bool) -> bool

  val confirm : title:string -> string -> (unit -> unit) -> unit
end

val main_window : GWindow.window

val open_text_view : GText.buffer -> GText.view

val open_toplevel_view : GText.buffer -> GText.view
