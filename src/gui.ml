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
  GButton.toolbar
  +< button_load
  +< button_save
  +< button_save_as
  +> button_quit

let main_view =
  GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ()

let toplevel_view =
  GBin.scrolled_window ~vpolicy:`AUTOMATIC ~hpolicy:`AUTOMATIC ()

let main_window =
  let win =
    GWindow.window ~title:"ocp-edit-simple" ~height:600 ~show:true
    +> (GPack.vbox
        ++< (toolbar, fun c o -> c#pack o)
        +> (GPack.hbox
            +< main_view
            +> toplevel_view))
  in
  win

let open_text_view buffer =
  Tools.debug "open text view";
  let font = Pango.Font.from_string "Monospace 10" in
  let view =
    GSourceView2.source_view
      ~source_buffer:buffer
      ~auto_indent:true
      ~highlight_current_line:true
      ~indent_on_tab:true
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`CHAR
      ()
  in
  List.iter main_view#remove main_view#children;
  main_view#add (view :> GObj.widget);
  view#misc#modify_base [`NORMAL, `NAME "grey20"];
  view#misc#modify_text [`NORMAL, `NAME "wheat"];
  view#misc#modify_font font;
  view#misc#set_size_chars ~width:81 ();
  view#misc#grab_focus ()

let open_toplevel_view top_buf =
  Tools.debug "open top view";
  let font = Pango.Font.from_string "Monospace 10" in
  let view =
    GSourceView2.source_view
      ~source_buffer:top_buf
      ~auto_indent:false
      ~highlight_current_line:false
      ~indent_on_tab:false
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`CHAR
      ~cursor_visible:false
      ~editable:false
      ()
  in
  toplevel_view#add (view :> GObj.widget);
  view#misc#modify_base [`NORMAL, `NAME "grey20"];
  view#misc#modify_text [`NORMAL, `NAME "wheat"];
  view#misc#modify_font font;
  view#misc#set_size_chars ~width:81 ()

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
  dialog#add_filter @@
    GFile.filter ~name:"OCaml source (*.ml)" ~patterns:["*.ml"] ();
  dialog#add_filter @@
    GFile.filter ~name:"All files" ~patterns:["*"] ();
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
  (* ignore @@ dialog#connect#response; *)
  ignore @@ dialog#run ();
  dialog#destroy ()

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
  let resp = ref true in
  ignore @@ dialog#connect#response ~callback:(function
  | `SAVE -> resp := () |> save_k
  | `QUIT -> resp := false
  | `CANCEL | `DELETE_EVENT -> resp := true);
  ignore @@ dialog#run ();
  dialog#destroy ();
  !resp

let confirm ~title message k =
  let dialog = GWindow.message_dialog
    ~title
    ~message
    ~use_markup:true
    ~message_type:`QUESTION
    ~buttons:GWindow.Buttons.yes_no
    ()
  in
  ignore @@ dialog#connect#response ~callback:(function
  | `YES -> () |> k
  | `NO | `DELETE_EVENT -> ());
  ignore @@ dialog#run ();
  dialog#destroy ()
