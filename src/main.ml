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

open Tools.Ops

module Buffer = OcamlBuffer (* fixme *)

let current_buffer = ref (Buffer.create Gui.open_text_view)
let toplevel_buffer =
  GSourceView2.source_buffer
    ?language:Buffer.GSourceView_params.syntax
    ?style_scheme:Buffer.GSourceView_params.style
    ~highlight_matching_brackets:true
    ~highlight_syntax:true
    ?undo_manager:None
    ~tag_table:Buffer.Tags.table
    ()

let rec protect ?(loop=false) f x =
  try
    f x
  with
  | Tools.Recoverable_error message ->
      Gui.Dialogs.error ~title:"Error" message;
      if loop then protect f x
  | exc ->
      Gui.Dialogs.error ~title:"Fatal error"
        (Printf.sprintf"<b>Uncaught exception:</b>\n\n%s"
           (Printexc.to_string exc));
      exit 10

module Actions = struct
  let load_file name =
    protect (Tools.File.load name) @@ fun contents ->
      let buf = Buffer.create ~name ~contents Gui.open_text_view in
      current_buffer := buf

  let confirm_discard k =
    if Buffer.is_modified !current_buffer then
      Gui.Dialogs.confirm ~title:"Please confirm"
        (Printf.sprintf "Discard your changes to %s ?"
         @@ Buffer.filename_default
           ~default:"the current file" !current_buffer)
      @@ k
    else k ()

  let load_dialog () =
    confirm_discard @@ fun () ->
      Gui.Dialogs.choose_file `OPEN load_file

  let save_to_file name () =
    let contents = Buffer.contents !current_buffer in
    protect (Tools.File.save contents name) @@ fun () ->
      Buffer.set_filename !current_buffer name;
      Buffer.unmodify !current_buffer

  let save_to_file_ask ?name () = match name with
    | Some n -> save_to_file n ()
    | None ->
      Gui.Dialogs.choose_file `SAVE  @@ fun name ->
        if Sys.file_exists name then
          Gui.Dialogs.confirm ~title:"Overwrite ?"
            (Printf.sprintf "File %s already exists. Overwrite ?" name)
          @@ save_to_file name
        else
          save_to_file name ()

  let new_empty () =
    confirm_discard @@ fun () ->
      current_buffer := Buffer.create Gui.open_text_view

  let check_before_quit _ =
    Buffer.is_modified !current_buffer &&
      Gui.Dialogs.quit (Buffer.filename !current_buffer) @@ fun () ->
        save_to_file_ask ?name:(Buffer.filename !current_buffer) ();
        Buffer.is_modified !current_buffer
end

let _bind_actions =
  Gui.Controls.bind `NEW Actions.new_empty;
  Gui.Controls.bind `OPEN Actions.load_dialog;
  Gui.Controls.bind `SAVE_AS Actions.save_to_file_ask;
  Gui.Controls.bind `SAVE (fun () ->
    Actions.save_to_file_ask ?name:(Buffer.filename !current_buffer) ());
  Gui.Controls.bind `QUIT (fun () ->
    if not (Actions.check_before_quit ()) then Gui.main_window#destroy ())

let _ =
  Tools.debug "Init done, showing main window";
  if Array.length Sys.argv > 1 then Actions.load_file Sys.argv.(1);
  TopUi.init_top_view current_buffer toplevel_buffer;
  Gui.main_window#show();
  Tools.debug "Setting up callback exception handler: %a" (fun ch s ->
    GtkSignal.user_handler := (fun exc ->
      Tools.debug "Exception in handler: %s at %s\n"
        (Printexc.to_string exc)
        (Printexc.get_backtrace ()));
    output_string ch s)
    "ok";
  protect ~loop:true GMain.main ()
