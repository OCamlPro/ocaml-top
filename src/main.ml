open Tools.Ops

module Buffer = struct
  type t = {
    mutable filename: string option;
    gbuffer: GText.buffer;
  }

  let is_modified buf = buf.gbuffer#modified

  let unmodify buf = buf.gbuffer#set_modified false

  let contents buf = buf.gbuffer#get_text ()

  let filename buf = buf.filename

  let set_filename buf name =
    buf.filename <- Some name

  let create ?name ?(contents="") view =
    let buf = {
      filename = name;
      gbuffer =
        if Glib.Utf8.validate contents then
          let gbuf = GText.buffer ~text:contents ()
          in view#set_buffer gbuf; gbuf
        else
          Tools.recover_error
            ("Could not open file %S because it contains invalid utf-8 characters. "
             ^^ "Please fix it or choose another file")
            (match name with Some n -> n | None -> "<unnamed>")
    } in
    unmodify buf;
    buf
end

let current_buffer = ref (Buffer.create Gui.text_view)

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
    let buf = Buffer.create ~name ~contents Gui.text_view in
    current_buffer := buf

let _bind_actions =
  ignore @@ Gui.button_load#connect#clicked
    ~callback:(fun () -> Gui.choose_file `OPEN load_file)
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
  Gui.main_window#show ();
  if Array.length (Sys.argv) > 1 then load_file Sys.argv.(1);
  protect ~loop:true GMain.main ()
