open Tools.Ops

type buffer = {
  filename: string option;
  gbuffer: GText.buffer;
}

let current_buffer = ref {
  filename = None;
  gbuffer = GText.buffer ();
}

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
    let buffer = GText.buffer ~text:contents () in
    current_buffer := {
      filename = Some name;
      gbuffer = buffer;
    };
    (* buffer#connect#changed ~callback: *)
    Gui.text_view#set_buffer buffer

let _bind_actions =
  ignore @@ Gui.button_load#connect#clicked
    ~callback:(fun () -> Gui.choose_file `OPEN load_file)
  ;
  let save_to_file name () =
    let contents = !current_buffer.gbuffer#get_text () in
    protect (Tools.File.save contents name) @@ fun () ->
      !current_buffer.gbuffer#set_modified false;
      current_buffer := {!current_buffer with
        filename = Some name;
      }
  in
  let save_to_file_ask ?name () = match name with
    | Some n -> save_to_file n ()
    | None -> Gui.choose_file `SAVE (fun name -> save_to_file name ())
  in
  ignore @@ Gui.button_save_as#connect#clicked ~callback:save_to_file_ask
  ;
  ignore @@ Gui.button_save#connect#clicked
    ~callback:(save_to_file_ask ?name:!current_buffer.filename)
  ;
  let quit () =
    if !current_buffer.gbuffer#modified then
      Gui.quit_dialog !current_buffer.filename
      @@ save_to_file_ask ?name:!current_buffer.filename
    else
      GMain.Main.quit ()
  in
  ignore @@ Gui.button_quit#connect#clicked ~callback:quit;
  ignore @@ Gui.main_window#connect#destroy ~callback:quit (* stop propagation?*)

let _ =
  Tools.debug "Init done, showing main window";
  Gui.main_window#show ();
  if Array.length (Sys.argv) > 1 then load_file Sys.argv.(1);
  protect ~loop:true GMain.main ()
