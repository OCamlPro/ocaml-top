let load_file filename =
  try
    let text = Tools.file_contents filename in
    let buffer = GText.buffer ~text () in
    Gui.text_view#set_buffer buffer
  with
    Unix.Unix_error _ | Sys_error _ ->
      failwith ("Could not load file"^filename)

let _ =
  try
    Gui.main_window#show ();
    load_file Sys.argv.(1);
    GMain.main ()
  with
    exc ->
      let dialog = GWindow.message_dialog
        ~title:"Fatal error"
        ~message:(Printf.sprintf"<b>Uncaught exception:</b>\n\n%s" (Printexc.to_string exc))
        ~use_markup:true
        ~message_type:`ERROR
        ~buttons:(GWindow.Buttons.close)
        ()
      in
      ignore (dialog#run ());
      exit 10
