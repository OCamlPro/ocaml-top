let _ = GtkMain.Main.init()

let toolbar =
  let button_save = GButton.button ~label:"Save" () in
  let bar = GPack.button_box `HORIZONTAL () in
  bar#pack (button_save :> GObj.widget);
  bar

let text_view =
  let view = GText.view () in
  view#misc#modify_font_by_name "Fixed 10";
  view

let main_window =
  let inside = GPack.vbox () in
  inside#add (toolbar :> GObj.widget);
  let scroll_text = GBin.scrolled_window () in
  scroll_text#add (text_view :> GObj.widget);
  inside#add (scroll_text :> GObj.widget);
  let win =
    GWindow.window ~title:"ocp-edit-simple" ~width:640 ~height:480 ()
  in
  win#add (inside :> GObj.widget);
  ignore (win#connect#destroy ~callback:GMain.Main.quit);
  win
