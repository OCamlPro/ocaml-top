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

let rec protect parent ?(loop=false) ?(err = fun () -> ()) f x =
  try
    f x
  with
  | Tools.Recoverable_error message ->
      Gui.Dialogs.error ~parent ~title:"Error" message;
      if loop then protect parent ~loop f x
      else () |> err
  | exc ->
      Gui.Dialogs.error ~parent ~title:"Fatal error"
        (Printf.sprintf"<b>Uncaught exception:</b>\n\n%s"
           (Printexc.to_string exc));
      exit 10

module BufActions = struct
  let load_file parent k name =
    protect parent (Tools.File.load name) @@ fun contents ->
      OBuf.create ~name ~contents ()
      |> k

  let confirm_discard parent k buf =
    if OBuf.is_modified buf then
      Gui.Dialogs.confirm ~parent ~title:"Please confirm"
        (Printf.sprintf "Discard your changes to %s ?"
         @@ OBuf.filename_default ~default:"the current file" buf)
        ~no:(fun () -> ())
      @@ k
    else k ()

  let load_dialog parent k =
    confirm_discard parent @@ fun () ->
      Gui.Dialogs.choose_file ~parent `OPEN
      @@ load_file parent
      @@ k

  let save_to_file parent ~ask k buf =
    let save name =
      let contents = OBuf.contents buf in
      protect parent (Tools.File.save contents name) @@ fun () ->
        OBuf.set_filename buf name;
        OBuf.unmodify buf
    in
    match OBuf.filename buf with
    | Some name when ask = false -> save name |> k
    | _ ->
        Gui.Dialogs.choose_file ~parent `SAVE @@ fun name ->
          let name =
            if String.contains name '.' then name else name ^ ".ml"
          in
          if Sys.file_exists name then
            Gui.Dialogs.confirm ~parent ~title:"Overwrite ?"
              (Printf.sprintf "File %s already exists. Overwrite ?" name)
            @@ fun () -> save name |> k
          else
            save name |> k

  let new_empty parent k =
    confirm_discard parent @@ fun () -> OBuf.create () |> k

  let quit main_window buf =
    if OBuf.is_modified buf then
      Gui.Dialogs.quit ~parent:main_window (OBuf.filename buf)
        ~save:(fun () k -> save_to_file main_window ~ask:false k buf)
        ~quit:(fun () -> main_window#destroy ())
    else main_window#destroy ()
end

module TopActions = struct
  let execute ~full top buf =
    OBuf.trigger_reindent buf OBuf.reindent_full
      ~cont:(fun () -> TopUi.topeval ~full buf top)

  let stop top buf =
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce
      ~where:(buf.OBuf.gbuffer#get_iter_at_mark buf.OBuf.eval_mark#coerce);
    match top.TopUi.process with Some process -> Top.stop process
                               | None -> ()

  let restart top buf =
    Gui.Controls.disable `RESTART;
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce
      ~where:buf.OBuf.gbuffer#start_iter;
    top.TopUi.buffer#delete
      ~start:top.TopUi.buffer#start_iter
      ~stop:top.TopUi.buffer#end_iter;
    match top.TopUi.process with Some process -> Top.kill process
                               | None -> ()

  let clear top _buf =
    top.TopUi.buffer#delete
      ~start:top.TopUi.buffer#start_iter
      ~stop:top.TopUi.buffer#end_iter;
end

module UIActions = struct
  let zoom value top_view src_view _top buf =
    let font = GPango.font_description_from_string !Cfg.font in
    let size = max 6 @@ min 24 @@ font#size / Pango.scale +  value in
    font#modify ~size:(size * Pango.scale) ();
    Gui.set_font src_view top_view font#to_string;
    OBuf.trigger_reindent buf OBuf.reindent_full

  let switch_theme _top_view _src_view top buf =
    Cfg.theme := if !Cfg.theme = "dark" then "light" else "dark";
    buf.OBuf.gbuffer#set_style_scheme (OBuf.GSourceView_params.style ());
    top.TopUi.buffer#set_style_scheme (OBuf.GSourceView_params.style ())

  let fullscreen window _top_view _src_view _top _buf =
    Gui.switch_fullscreen window
end

let init_code_view main_window buf =
  OBuf.setup_indent buf;
  let gbuf = buf.OBuf.gbuffer in
  let view = Gui.open_text_view gbuf in
  let set_title () =
    Gui.set_window_title main_window "%s%s"
      (OBuf.filename_default buf)
      (if gbuf#modified then "*" else "")
  in
  ignore @@ gbuf#connect#modified_changed ~callback:set_title;
  set_title ();
  view

let init ?name ?contents main_window =
  (* Initialize the source buffer and actions *)
  let buf_ref = ref (OBuf.create ?name ?contents ()) in
  let view_ref = ref (init_code_view main_window !buf_ref) in
  Completion.setup !buf_ref !view_ref Gui.index_msg;
  let get_buf k () = !buf_ref |> k in
  let set_buf buf =
    buf_ref := buf;
    view_ref := init_code_view main_window buf;
    Completion.setup buf !view_ref Gui.index_msg
  in
  let nil () = () in
  Gui.Controls.bind `NEW     @@ get_buf @@ BufActions.new_empty main_window @@ set_buf;
  Gui.Controls.bind `OPEN    @@ get_buf @@ BufActions.load_dialog main_window @@ set_buf;
  Gui.Controls.bind `SAVE_AS @@ get_buf @@ BufActions.save_to_file main_window ~ask:true
  @@ nil;
  Gui.Controls.bind `SAVE    @@ get_buf @@ BufActions.save_to_file main_window ~ask:false
  @@ nil;
  Gui.Controls.bind `QUIT    @@ get_buf @@ BufActions.quit main_window;
  (* Initialize the top-level buffer and actions *)
  let toplevel_buffer = TopUi.create_buffer () in
  let get_top f () = get_buf (f toplevel_buffer) () in
  Gui.Controls.bind `EXECUTE     @@ get_top @@ TopActions.execute ~full:false;
  Gui.Controls.bind `EXECUTE_ALL @@ get_top @@ TopActions.execute ~full:true;
  Gui.Controls.bind `STOP        @@ get_top @@ TopActions.stop;
  Gui.Controls.bind `RESTART     @@ get_top @@ TopActions.restart;
  Gui.Controls.bind `CLEAR       @@ get_top @@ TopActions.clear;
  (* Create the toplevel view *)
  let top_buf = toplevel_buffer.TopUi.buffer in
  let top_view = Gui.open_toplevel_view top_buf in
  (* UI actions *)
  let get_view f () = get_top (f top_view !view_ref) () in
  Gui.Controls.bind `ZOOM_IN @@ get_view @@ UIActions.zoom (+1);
  Gui.Controls.bind `ZOOM_OUT @@ get_view @@ UIActions.zoom (-1);
  Gui.Controls.bind `SELECT_COLOR @@ get_view @@ UIActions.switch_theme;
  Gui.Controls.bind `FULLSCREEN @@ get_view @@ UIActions.fullscreen main_window;
  Gui.set_font !view_ref top_view !Cfg.font;
  ignore @@ top_buf#connect#after#changed ~callback:(fun () ->
      ignore @@ GMain.Idle.add @@ fun () ->
        ignore @@ top_view#scroll_to_iter
            (top_buf#end_iter#set_line_offset 0);
        false);
  (* Start the toplevel *)
  let init () =
    let buf = !buf_ref in
    let where = buf.OBuf.gbuffer#start_iter in
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark#coerce ~where;
    buf.OBuf.gbuffer#move_mark buf.OBuf.eval_mark_end#coerce ~where
  in
  let status_change_hook =
    let show_spinner = TopUi.show_spinner toplevel_buffer top_view in
    function
    | Top.Dead | Top.Starting ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `EXECUTE_ALL;
        Gui.Controls.disable `RESTART;
        Gui.Controls.disable `STOP;
        show_spinner false
    | Top.Ready ->
        Gui.Controls.enable `EXECUTE;
        Gui.Controls.enable `EXECUTE_ALL;
        Gui.Controls.enable `RESTART;
        Gui.Controls.disable `STOP;
        Gui.top_msg "Ready.";
        show_spinner false
    | Top.Busy _ ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `EXECUTE_ALL;
        Gui.Controls.enable `RESTART;
        Gui.Controls.enable `STOP;
        Gui.top_msg "Working...";
        show_spinner true
  in
  status_change_hook Top.Starting;
  TopUi.top_start ~init ~status_change_hook toplevel_buffer;
  (* Don't worry about the change hook, it won't be triggered
     anymore once the gtk main loop has ended. *)
  at_exit (fun () ->
      match toplevel_buffer.TopUi.process with
      | Some p -> Top.kill p
      | None -> ());
  Tools.debug "Init done, showing main window"

let args =
  let set_ocaml_cmd s =
    Cfg.ocaml_opts := [];
    Cfg.ocaml_cmd := s
  in
  Arg.align [
    "-ocaml", Arg.String set_ocaml_cmd,
    "COMMAND Set the ocaml toplevel executable. Warning: the default is \n\
    \        `-ocaml ocamlrun --- ocaml`, which is required on Windows for\n\
    \        proper process handling (don't run the ocaml bytecode directly).";
    "-font", Arg.Set_string Cfg.font,
    "FONT Choose the font to use, as for Gtk settings. It must be monospace";
    "-light", Arg.Unit (fun () -> Cfg.theme := "light"),
    " Set a light theme instead of the default dark one";
    "-datadir", Arg.Set_string Cfg.datadir,
    "PATH Directory where to find ocaml-top resources";
    (* "--" is eaten by gtk, that's why we need "---" *)
    "---",
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
  let _reloc_gtk_conf_on_windows =
    if Cfg.os = Cfg.Windows then
      let gtkrc = Filename.concat !Cfg.datadir "gtkrc" in
      (try Glib.setenv "GTK_PATH" !Cfg.datadir true with Not_found -> ());
      if Sys.file_exists gtkrc then GtkMain.Rc.parse ~file:gtkrc
  in
  let main_window = Gui.main_window ()
  in
  let create () =
    match !file with
    | Some name ->
        let rec load name =
          protect main_window (Tools.File.load name)
            ~err:(fun () ->
                Gui.Dialogs.choose_file ~parent:main_window `OPEN
                  ~cancel:(fun () ->
                      protect main_window init main_window;
                      main_window#show())
                @@ load)
          @@ fun contents ->
            protect main_window (init ~name ~contents) main_window;
            main_window#show();
        in
        load name
    | None ->
        (protect main_window init main_window; main_window#show())
  in
  ignore @@ GMain.Idle.add (fun () -> create (); false);
  Sys.set_signal Sys.sigint
    (Sys.Signal_handle (fun _ -> main_window#destroy ()));
  protect main_window ~loop:true GMain.main ();
  Tools.debug "Goodbye !"
