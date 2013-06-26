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

module OBuf = OcamlBuffer

let rec protect ?(loop=false) ?(err = fun () -> ()) f x =
  try
    f x
  with
  | Tools.Recoverable_error message ->
      Gui.Dialogs.error ~title:"Error" message;
      if loop then protect ~loop f x
      else () |> err
  | exc ->
      Gui.Dialogs.error ~title:"Fatal error"
        (Printf.sprintf"<b>Uncaught exception:</b>\n\n%s"
           (Printexc.to_string exc));
      exit 10

module BufActions = struct
  let load_file k name =
    protect (Tools.File.load name) @@ fun contents ->
      OBuf.create ~name ~contents ()
      |> k

  let confirm_discard k buf =
    if OBuf.is_modified buf then
      Gui.Dialogs.confirm ~title:"Please confirm"
        (Printf.sprintf "Discard your changes to %s ?"
         @@ OBuf.filename_default ~default:"the current file" buf)
        ~no:(fun () -> ())
      @@ k
    else k ()

  let load_dialog k =
    confirm_discard @@ fun () ->
      Gui.Dialogs.choose_file `OPEN
      @@ load_file
      @@ k

  let save_to_file ~ask k buf =
    let save name =
      let contents = OBuf.contents buf in
      protect (Tools.File.save contents name) @@ fun () ->
        OBuf.set_filename buf name;
        OBuf.unmodify buf
    in
    match OBuf.filename buf with
    | Some name when ask = false -> save name |> k
    | _ ->
        Gui.Dialogs.choose_file `SAVE @@ fun name ->
          if Sys.file_exists name then
            Gui.Dialogs.confirm ~title:"Overwrite ?"
              (Printf.sprintf "File %s already exists. Overwrite ?" name)
            @@ fun () -> save name |> k
          else
            save name |> k

  let new_empty k =
    confirm_discard @@ fun () -> OBuf.create () |> k

  let quit buf =
    if OBuf.is_modified buf then
      Gui.Dialogs.quit (OBuf.filename buf)
        ~save:(fun () k -> save_to_file ~ask:false k buf)
        ~quit:(fun () -> Gui.main_window#destroy ())
    else Gui.main_window#destroy ()
end

module TopActions = struct
  open TopUi

  let execute ~full top buf =
    OBuf.trigger_reindent buf OBuf.Reindent_full
      ~cont:(fun () -> topeval ~full buf top)

  let stop top buf =
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce
      ~where:(buf.OBuf.gbuffer#get_iter_at_mark buf.OBuf.eval_mark#coerce);
    match top.process with Some process -> Top.stop process
                         | None -> ()

  let restart top buf =
    Gui.Controls.disable `RESTART;
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce
      ~where:buf.OBuf.gbuffer#start_iter;
    top.buffer#delete
      ~start:top.buffer#start_iter
      ~stop:top.buffer#end_iter;
    match top.process with Some process -> Top.kill process
                         | None -> ()

  let clear top buf =
    top.TopUi.buffer#delete
      ~start:top.buffer#start_iter
      ~stop:top.buffer#end_iter;
end

let init_code_view  buf =
  OBuf.setup_indent buf;
  let gbuf = buf.OBuf.gbuffer in
  let view = Gui.open_text_view gbuf in
  ignore @@ gbuf#connect#modified_changed ~callback:(fun () ->
      Gui.set_window_title "%s%s"
        (OBuf.filename_default buf)
        (if gbuf#modified then "*" else "")
    );
  view

let init ?name ?contents () =
  (* Initialize the source buffer and actions *)
  let buf_ref = ref (OBuf.create ?name ?contents ()) in
  let view_ref = ref (init_code_view !buf_ref) in
  Completion.setup !buf_ref !view_ref;
  let get_buf k () = !buf_ref |> k in
  let set_buf buf =
    buf_ref := buf;
    view_ref := init_code_view buf;
    Completion.setup buf !view_ref
  in
  let nil () = () in
  Gui.Controls.bind `NEW     @@ get_buf @@ BufActions.new_empty @@ set_buf;
  Gui.Controls.bind `OPEN    @@ get_buf @@ BufActions.load_dialog @@ set_buf;
  Gui.Controls.bind `SAVE_AS @@ get_buf @@ BufActions.save_to_file ~ask:true
  @@ nil;
  Gui.Controls.bind `SAVE    @@ get_buf @@ BufActions.save_to_file ~ask:false
  @@ nil;
  Gui.Controls.bind `QUIT    @@ get_buf @@ BufActions.quit;
  (* Initialize the top-level buffer and actions *)
  let toplevel_buffer = TopUi.create_buffer () in
  let get_top f () = get_buf (f toplevel_buffer) () in
  Gui.Controls.bind `EXECUTE     @@ get_top @@ TopActions.execute ~full:false;
  Gui.Controls.bind `EXECUTE_ALL @@ get_top @@ TopActions.execute ~full:true;
  Gui.Controls.bind `STOP        @@ get_top @@ TopActions.stop;
  Gui.Controls.bind `RESTART     @@ get_top @@ TopActions.restart;
  Gui.Controls.bind `CLEAR       @@ get_top @@ TopActions.clear;
  (* Start the toplevel *)
  let init () =
    let buf = !buf_ref in
    let where = buf.OBuf.gbuffer#start_iter in
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark#coerce ~where;
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce ~where
  in
  let status_change_hook = function
    | Top.Dead | Top.Starting ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `EXECUTE_ALL;
        Gui.Controls.disable `RESTART;
        Gui.Controls.disable `STOP
    | Top.Ready ->
        Gui.Controls.enable `EXECUTE;
        Gui.Controls.enable `EXECUTE_ALL;
        Gui.Controls.enable `RESTART;
        Gui.Controls.disable `STOP
    | Top.Busy _ ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `EXECUTE_ALL;
        Gui.Controls.enable `RESTART;
        Gui.Controls.enable `STOP
  in
  TopUi.top_start ~init ~status_change_hook toplevel_buffer;
  (* Don't worry about the change hook, it won't be triggered
     anymore once the gtk main loop has ended. *)
  at_exit (fun () ->
      match toplevel_buffer.TopUi.process with
      | Some p -> Top.kill p
      | None -> ());
  (* Create the toplevel view *)
  let top_buf = toplevel_buffer.TopUi.buffer in
  let top_view = Gui.open_toplevel_view top_buf in
  ignore @@ top_buf#connect#changed ~callback:(fun () ->
      ignore @@ top_view#scroll_to_iter (top_buf#end_iter#set_line_offset 0));
  Tools.debug "Init done, showing main window"

let args =
  Arg.align [
    "--ocaml", Arg.Set_string Cfg.ocaml_path,
    "PATH Set the ocaml toplevel executable";
    "--font", Arg.Set_string Cfg.font,
    "FONT Choose the font to use, as for Gtk settings. It must be monospace";
    "--",
    Arg.Rest (fun s -> Cfg.ocaml_opts := !Cfg.ocaml_opts @ [s]),
    " Remaining arguments are passed to the ocaml toplevel"
  ]

let _ =
  let file = ref None in
  Arg.parse args
    (fun s -> match !file with None -> file := Some s
                             | Some _ -> raise (Arg.Bad ("extra parameter "^s)))
    "ocaml-top [file]\n\
    \  Simple graphical ocaml code editor designed for top-level interaction.\n\
    \  Options:"
  ;
  Tools.debug "Setting up callback exception handler: %a" (fun ch s ->
    GtkSignal.user_handler := (fun exc ->
      Tools.debug "Exception in handler: %s at %s\n"
        (Printexc.to_string exc)
        (Printexc.get_backtrace ()));
    output_string ch s)
    "ok"
  ;
  let create () =
    match !file with
    | Some name ->
        let rec load name =
          protect (Tools.File.load name)
            ~err:(fun () ->
                Gui.Dialogs.choose_file `OPEN
                  ~cancel:(fun () -> protect init (); Gui.main_window#show())
                @@ load)
          @@ fun contents ->
            protect (init ~name ~contents) ();
            Gui.main_window#show();
        in
        load name
    | None ->
        (protect init (); Gui.main_window#show())
  in
  ignore @@ GMain.Idle.add (fun () -> create (); false);
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> Gui.main_window#destroy ()));
  protect ~loop:true GMain.main ();
  Tools.debug "Goodbye !"
