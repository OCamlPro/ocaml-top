open Tools.Ops

let set_window_title fmt =
  Printf.ksprintf Gui.main_window#set_title (fmt ^^ " - ocp-edit-simple")

module GSourceView_params = struct
  let syntax =
    (GSourceView2.source_language_manager ~default:true)
      #language "objective-caml"
  let style =
    (GSourceView2.source_style_scheme_manager ~default:true)
      #style_scheme "cobalt"
end

module Buffer = struct
  type t = {
    mutable filename: string option;
    gbuffer: GSourceView2.source_buffer;
    view: GSourceView2.source_view;
  }

  let contents buf = buf.gbuffer#get_text ()

  let is_modified buf = buf.gbuffer#modified

  let unmodify buf = buf.gbuffer#set_modified false

  let filename buf = buf.filename

  let filename_default ?(default="<unnamed.ml>") buf =
    match buf.filename with
    | Some f -> Filename.basename f
    | None -> default

  let set_filename buf name =
    buf.filename <- Some name

  module Tags = struct
    let phrase =
      Tools.debug "phrase";
      let t = GText.tag ~name:"phrase" () in
      t#set_property (`FOREGROUND "grey80");
      (* t#set_property (`BACKGROUND "black"); *)
      (* property paragraph-background colors entire line, but it
         was introduced in gtk 2.8 and isn't yet in lablgtk... *)
      t#set_property (`INDENT 16); (* fixme: 2*font-width *)
      t

    let table =
      Tools.debug "table";
      let table = GText.tag_table () in
      table#add phrase#as_tag;
      table
  end

  let create ?name ?(contents="")
      (mkview: GSourceView2.source_buffer -> GSourceView2.source_view) =
    let gbuffer =
      if not (Glib.Utf8.validate contents) then
        Tools.recover_error
          ("Could not open file %s because it contains invalid utf-8 "
           ^^ "characters. Please fix it or choose another file")
          (match name with Some n -> n | None -> "<unnamed>");
      GSourceView2.source_buffer
        ~text:contents
        ?language:GSourceView_params.syntax
        ?style_scheme:GSourceView_params.style
        ~highlight_matching_brackets:true
        ~highlight_syntax:true
        ~tag_table:Tags.table
        ()
    in
    (* workaround: if we don't do this, loading of the file can be undone *)
    gbuffer#begin_not_undoable_action ();
    gbuffer#place_cursor ~where:gbuffer#start_iter;
    let view = mkview gbuffer in
    let t = { filename = name; gbuffer; view } in
    ignore @@ gbuffer#connect#modified_changed ~callback:(fun () ->
      set_window_title "%s%s" (filename_default t) @@
        if gbuffer#modified then "*" else "");
    unmodify t;
    gbuffer#end_not_undoable_action ();
    t

  let get_selection buf =
    let gbuf = buf.gbuffer in
    if gbuf#has_selection then
      let start,stop = gbuf#selection_bounds in
      Some (gbuf#get_text ~start ~stop ())
    else
      None
end

let current_buffer = ref (Buffer.create Gui.open_text_view)
let toplevel_buffer =
  GSourceView2.source_buffer
    ?language:GSourceView_params.syntax
    ?style_scheme:GSourceView_params.style
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

let init_top_view () =
  let top_view = Gui.open_toplevel_view toplevel_buffer in
  let topeval top =
    let phrase = match Buffer.get_selection !current_buffer with
      | Some p -> p
      | None -> Buffer.contents !current_buffer
    in
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n# ";
    toplevel_buffer#insert
      ~iter:toplevel_buffer#end_iter
      ~tags:[Buffer.Tags.phrase]
      phrase;
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n";
    if phrase.[String.length phrase - 1] <> '\n' then
      toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n";
    ignore @@ top_view#scroll_to_iter toplevel_buffer#end_iter;
    Top.query top phrase
  in
  let top_display response =
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter response;
    ignore @@ top_view#scroll_to_iter toplevel_buffer#end_iter
  in
  let schedule f = GMain.Idle.add @@ fun () -> f (); false in
  let top = Top.start schedule top_display in
  Gui.Controls.bind `EXECUTE @@ fun () -> topeval top;
  Gui.Controls.bind `STOP @@ fun () -> Top.stop top

let _ =
  Tools.debug "Init done, showing main window";
  if Array.length Sys.argv > 1 then Actions.load_file Sys.argv.(1);
  init_top_view ();
  Gui.main_window#show();
  protect ~loop:true GMain.main ()
