open Tools.Ops
let _ = GtkMain.Main.init()

(* Some operators to make the layout building look more functional and
   tree-like *)
(* Apply an object builder, add a widget, and return the same interface as the
   object builder so that it can be chained *)
let (+<) builder obj =
  let o = builder () in
  o#add (obj :> GObj.widget);
  fun () -> o

(* The same but closed: returns the object *)
let (+>) builder obj =
  (builder +< obj) ()

(* The same as (+<), but for when you need to specify the packing function used
   to add the widget to the object *)
let (++<) builder (obj, pack) =
  let o = builder () in
  (pack o) (obj :> GObj.widget);
  fun () -> o

(* Same as (++<) but closed *)
let (++>) builder obj =
  (builder ++< obj) ()

let button_load = GButton.button ~stock:`OPEN ()
and button_save = GButton.button ~stock:`SAVE ()
and button_save_as = GButton.button ~stock:`SAVE_AS ()
and button_quit = GButton.button ~stock:`QUIT ()

let toolbar =
  GButton.toolbar (* ~orientation:`HORIZONTAL *)
  +< button_load
  +< button_save
  +< button_save_as
  +> button_quit

let text_view =
  let view = GText.view () in
  view#misc#modify_base [`NORMAL, `NAME "grey20"];
  view#misc#modify_text [`NORMAL, `NAME "wheat"];
  view#misc#modify_font (Pango.Font.from_string "Monospace 10");
  view

let main_window =
  let win =
    GWindow.window ~title:"ocp-edit-simple" ~width:640 ~height:480
    +> (GPack.vbox
        ++< (toolbar, fun c o -> c#pack ~expand:false o)
        +> (GBin.scrolled_window
            +> text_view))
  in
  win

let choose_file action callback =
  let title, button_label = match action with
    | `OPEN -> "Please choose file to load", "Load"
    | `SAVE -> "Please choose file to save to", "Save"
  in
  let dialog =
    GWindow.file_chooser_dialog
      ~title
      ~action:(action :> GtkEnums.file_chooser_action) ()
  in
  let callback x =
    match x with
    | `CANCEL | `DELETE_EVENT -> dialog#destroy ()
    | `APPLY ->
      match dialog#filename with
      | None -> failwith "None selected"
      | Some name -> callback name; dialog#destroy ()
  in
  dialog#add_select_button_stock (action :> GtkStock.id) `APPLY;
  dialog#add_button_stock `CANCEL `CANCEL;
  ignore @@ dialog#connect#response ~callback;
  dialog#show ()

let error_message ~title message =
  let dialog = GWindow.message_dialog
    ~title
    ~message
    ~use_markup:true
    ~message_type:`ERROR
    ~buttons:GWindow.Buttons.close
    ()
  in
  ignore @@ dialog#connect#response ~callback:(fun _ -> dialog#destroy ());
  ignore @@ dialog#run ()

let quit_dialog filename save_k =
  let filename = match filename with
    | Some f -> Printf.sprintf "File %S" f
    | None -> "Current buffer"
  in
  let dialog = GWindow.dialog ~title:"Quit" () in
  dialog#vbox#add
    (GMisc.label
       ~markup:(filename^" contains unsaved changes. What to do ?") ()
    :> GObj.widget);
  dialog#add_button_stock `SAVE `SAVE;
  dialog#add_button_stock `QUIT `QUIT;
  dialog#add_button_stock `CANCEL `CANCEL;
  (* let dialog = *)
  (*   GWindow.dialog ~title:"Quit" *)
  (*   +< GMisc.label *)
  (*     ~markup:(filename^" contains unsaved changes. What to do ?") *)
  (*     () *)
  (*   +> (GPack.button_box `HORIZONTAL *)
  (*       +< GButton.button ~stock:`SAVE () *)
  (*       +< GButton.button ~stock:`QUIT () *)
  (*       +> GButton.button ~stock:`CANCEL ()) *)
  (* in *)
  ignore @@ dialog#connect#response ~callback:(function
  | `SAVE -> save_k (); GMain.Main.quit ()
  | `QUIT -> GMain.Main.quit ()
  | `CANCEL | `DELETE_EVENT -> dialog#destroy ());
  ignore @@ dialog#run ()
