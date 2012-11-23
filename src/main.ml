open Tools.Ops

let set_window_title fmt =
  Printf.ksprintf Gui.main_window#set_title (fmt ^^ " - ocp-edit-simple")

module Buffer = struct
  type t = {
    mutable filename: string option;
    gbuffer: GSourceView2.source_buffer
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

  let create ?name ?(contents="") (k: GSourceView2.source_buffer -> unit) =
    Tools.debug "create %s <<%s>>" (match name with None -> "None" | Some x -> x) contents;
    let gbuffer =
      let language =
        (GSourceView2.source_language_manager ~default:true)
          #language "objective-caml"
      in
      let style_scheme =
        (GSourceView2.source_style_scheme_manager ~default:true)
          #style_scheme "cobalt"
      in
      if Glib.Utf8.validate contents then
        GSourceView2.source_buffer
          ~text:contents
          ?language
          ?style_scheme
          ~highlight_matching_brackets:true
          ~highlight_syntax:true
          ()
      else
        Tools.recover_error
          ("Could not open file %S because it contains invalid utf-8 "
           ^^ "characters. Please fix it or choose another file")
          (match name with Some n -> n | None -> "<unnamed>")
    in
    (* workaround: if we don't do this, loading of the file can be undone *)
    gbuffer#begin_not_undoable_action ();
    gbuffer#place_cursor ~where:gbuffer#start_iter;
    let t = { filename = name; gbuffer } in
    ignore @@ gbuffer#connect#modified_changed ~callback:(fun () ->
      set_window_title "%s%s" (filename_default t) @@
        if gbuffer#modified then "*" else "");
    unmodify t;
    gbuffer |> k;
    gbuffer#end_not_undoable_action ();
    t

end

let current_buffer = ref (Buffer.create Gui.open_text_view)

let rec protect ?(loop=false) f x =
  try
    f x
  with
  | Tools.Recoverable_error message ->
      Gui.error_message ~title:"Error" message;
      if loop then protect f x
  | exc ->
      Gui.error_message ~title:"Fatal error"
        (Printf.sprintf"<b>Uncaught exception:</b>\n\n%s"
           (Printexc.to_string exc));
      exit 10

let load_file name =
  protect (Tools.File.load name) @@ fun contents ->
    let buf = Buffer.create ~name ~contents Gui.open_text_view in
    current_buffer := buf

let _bind_actions =
  let load_dialog () =
    let dialog () = Gui.choose_file `OPEN load_file in
    if Buffer.is_modified !current_buffer then
      Gui.confirm ~title:"File modified"
        (Printf.sprintf "Discard your changes to %S ?"
         @@ Buffer.filename_default ~default:"the current buffer" !current_buffer)
      @@ dialog
    else
      dialog ()
  in
  ignore @@ Gui.button_load#connect#clicked ~callback:load_dialog
  ;
  let save_to_file name () =
    let contents = Buffer.contents !current_buffer in
    protect (Tools.File.save contents name) @@ fun () ->
      Buffer.set_filename !current_buffer name;
      Buffer.unmodify !current_buffer;
  in
  let save_to_file_ask ?name () = match name with
    | Some n -> save_to_file n ()
    | None ->
      Gui.choose_file `SAVE  @@ fun name ->
        if Sys.file_exists name then
          Gui.confirm ~title:"Overwrite ?"
            (Printf.sprintf "File %S already exists. Overwrite ?" name)
          @@ save_to_file name
        else
          save_to_file name ()
  in
  ignore @@ Gui.button_save_as#connect#clicked ~callback:save_to_file_ask
  ;
  ignore @@ Gui.button_save#connect#clicked
    ~callback:(fun () -> save_to_file_ask ?name:(Buffer.filename !current_buffer) ())
  ;
  let check_before_quit _ =
    Buffer.is_modified !current_buffer &&
      Gui.quit_dialog (Buffer.filename !current_buffer) @@ fun () ->
        save_to_file_ask ?name:(Buffer.filename !current_buffer) ();
        Buffer.is_modified !current_buffer
  in
  ignore @@ Gui.button_quit#connect#clicked
    ~callback:(fun () ->
      if not (check_before_quit ()) then Gui.main_window#destroy ());
  ignore @@ Gui.main_window#event#connect#delete
    ~callback:check_before_quit;
  ignore @@ Gui.main_window#connect#destroy
    ~callback:GMain.Main.quit


let _ =
  Tools.debug "Init done, showing main window";
  if Array.length (Sys.argv) > 1 then load_file Sys.argv.(1);
  protect ~loop:true GMain.main ()
