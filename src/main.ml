let _ = GtkMain.Main.init()

let main_window =
  GWindow.window ~title:"ocp-edit-simple" ~width:640 ~height:480 ()

let text_view =
  let scroll = GBin.scrolled_window ~packing:main_window#add () in
  let view = GText.view ~packing:scroll#add () in
  view#misc#modify_font_by_name "Fixed 10";
  view

let file_contents filename =
  let size = (Unix.stat filename).Unix.st_size in
  if size > Sys.max_string_length then failwith "Maximum file size exceeded";
  let ic = open_in filename in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let load_file filename =
  try
    let text = file_contents filename in
    let buffer = GText.buffer ~text () in
    text_view#set_buffer buffer
  with
    Unix.Unix_error _ | Sys_error _ ->
      failwith ("Could not load file"^filename)

let _ =
  try
    main_window#connect#destroy ~callback:GMain.Main.quit;
    main_window#show ();
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
