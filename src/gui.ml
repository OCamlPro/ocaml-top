open Tools.Ops

let _ = GtkMain.Main.init()

module Controls = struct
  type t = [ `NEW | `OPEN | `SAVE | `SAVE_AS
           | `EXECUTE | `STOP | `CLEAR | `RESTART
           | `QUIT ]

  let stock: t -> GtkStock.id = function
    | `RESTART -> `REFRESH
    | #GtkStock.id as id -> id

  let to_string command = GtkStock.convert_id (stock command)

  let help: t -> string * string = function
    | `NEW -> "New","Create a new file"
    | `OPEN -> "Open...","Select an existing file to edit"
    | `SAVE -> "Save","Save the current file"
    | `SAVE_AS -> "Save as...","Select a file to save the current program to"
    | `EXECUTE -> "Run","Run the current program, or the selection if any"
    | `STOP -> "Stop","Stop ongoing program execution"
    | `RESTART -> "Restart","Terminate the current toplevel and start a new one"
    | `CLEAR -> "Clear","Clear the toplevel window history"
    | `QUIT -> "Quit","Quit ocp-edit-simple"

  (* We could use lablgtk's action groups as well. But better map from an open
     variant than from strings... *)
  let signal =
    let controls : (t,GAction.action) Hashtbl.t = Hashtbl.create 17
    in fun t ->
      try Hashtbl.find controls t
      with Not_found ->
          let c = GAction.action ~name:(to_string t) () in
          c#set_stock_id (stock t);
          Hashtbl.add controls t c;
          c

  let bind command action =
    ignore @@ (signal command)#connect#activate ~callback:action

  let trigger command =
    Tools.debug "Event triggered: %s" @@ to_string command;
    ignore @@ (signal command)#activate ()

  let add_trigger command widget =
    (signal command)#connect_proxy (widget :> GObj.widget)

  let enable command =
    (signal command)#set_sensitive true

  let disable command =
    (signal command)#set_sensitive false

end

(* use `ALWAYS for vertical scrollbars, otherwise it is possible to trigger a
   bug in GTK that locks the mouse when resizing the panes *)
let main_view =
  GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`NEVER ()

let toplevel_view =
  GBin.scrolled_window ~vpolicy:`ALWAYS ~hpolicy:`ALWAYS ()

type shortcut_mods = [ `CONTROL | `SHIFT | `META | `SUPER | `HYPER ]
let shortcuts = [
  ([`CONTROL], GdkKeysyms._n),      `NEW;
  ([`CONTROL], GdkKeysyms._o),      `OPEN;
  ([`CONTROL], GdkKeysyms._s),      `SAVE;
  ([`CONTROL], GdkKeysyms._e),      `EXECUTE;
  ([],         GdkKeysyms._Escape), `STOP;
  ([`CONTROL], GdkKeysyms._q),      `QUIT;
]

let add objs container =
  List.iter (fun o -> container#add (o :> GObj.widget)) objs;
  container

let pack objs (container: GPack.box) =
  List.iter (fun o -> container#pack o) objs;
  container

let as_widget o = (o :> GObj.widget)

let main_window =
  let logo = GdkPixbuf.from_file "data/logo.png" in
  let tooltips = GData.tooltips () in
  let mkbutton ctrl =
    let label,text = Controls.help ctrl in
    let btn = GButton.tool_button ~stock:(Controls.stock ctrl) ~label () in
    Controls.add_trigger ctrl btn;
    btn#set_label label;
    tooltips#set_tip ~text (btn :> GObj.widget);
    (* ignore @@ btn#connect#clicked ~callback:(fun () -> Controls.trigger ctrl); *)
    (btn :> GObj.widget)
  in
  let win =
    GWindow.window
      ~title:"ocp-simple-edit"
      ~height:600 ~allow_shrink:true (* ~width:800 ~show:true *)
      ~icon:logo
      ()
    |> add [
      GPack.vbox ()
      |> pack [
        GButton.toolbar ()
        |> add [
          mkbutton `NEW;
          mkbutton `OPEN;
          mkbutton `SAVE;
          mkbutton `SAVE_AS;
          (GButton.separator_tool_item () :> GObj.widget);
          mkbutton `EXECUTE;
          mkbutton `STOP;
          mkbutton `CLEAR;
          mkbutton `RESTART;
          (GButton.separator_tool_item () :> GObj.widget);
          mkbutton `QUIT;
        ]
        |> as_widget;
      ]
      |> add [
        GPack.paned `HORIZONTAL ()
        |> add [
          (* GBin.frame ~label:"Source editor" ~shadow_type:`IN () *)
          (* |> add [ *) main_view (* ] *);
          (* GBin.frame ~label:"OCaml interaction" ~shadow_type:`IN () *)
          (* |> add [ *) toplevel_view (* ]; *)
        ];
      ];
    ]
  in
  ignore @@ win#event#connect#delete
    ~callback:(fun _ -> Controls.trigger `QUIT; true);
  ignore @@ win#connect#destroy ~callback:GMain.quit;
  ignore @@ win#event#connect#key_press ~callback:(fun ev ->
    let state = GdkEvent.Key.state ev
      |> List.filter (function #shortcut_mods -> true | _ -> false)
    in
    try
      Controls.trigger @@ List.assoc (state, GdkEvent.Key.keyval ev) shortcuts;
      false
    with
      | Not_found -> false);
  win

let set_window_title fmt =
  Printf.ksprintf main_window#set_title (fmt ^^ " - ocp-edit-simple")

let open_text_view buffer =
  Tools.debug "open text view";
  let view =
    GSourceView2.source_view
      ~source_buffer:buffer
      ~auto_indent:true
      ~highlight_current_line:true
      ~indent_on_tab:false
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`CHAR
      ~show_right_margin:true
      ~show_line_marks:true
      ~show_line_numbers:true
      ()
  in
  List.iter main_view#remove main_view#children;
  view#misc#modify_font_by_name
    "Consolas,Courier new,Vera sans,Monospace Mono 10";

  main_view#add (view :> GObj.widget);
  (* view#misc#modify_base [`NORMAL, `NAME "grey20"]; *)
  (* view#misc#modify_text [`NORMAL, `NAME "wheat"]; *)
  view#misc#set_size_chars ~width:84 ();
  view#misc#grab_focus ();
  view

let open_toplevel_view top_buf =
  Tools.debug "open top view";
  let view =
    GSourceView2.source_view
      ~source_buffer:top_buf
      ~auto_indent:false
      ~highlight_current_line:false
      ~indent_on_tab:false
      ~indent_width:2
      ~accepts_tab:false
      ~wrap_mode:`NONE
      ~cursor_visible:false
      ~editable:false
      ()
  in
  view#misc#modify_font_by_name
    "Consolas,Courier new,Vera sans,Monospace Mono 10";
  toplevel_view#add (view :> GObj.widget);
  (* view#misc#modify_base [`NORMAL, `NAME "grey20"]; *)
  (* view#misc#modify_text [`NORMAL, `NAME "wheat"]; *)
  view#misc#set_size_chars ~width:81 ();
  view

module Dialogs = struct

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

  let error ~title message =
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

  let quit filename save_k =
    let filename = match filename with
      | Some f -> Printf.sprintf "File %S" f
      | None -> "Current buffer"
    in
    let dialog = GWindow.dialog ~title:"Quit" () in
    let txt =
      let frame = GBin.frame ~border_width:40 ~shadow_type:`NONE () in
      frame#add
        (GMisc.label
           ~markup:(filename^" contains unsaved changes. What to do ?") ()
         :> GObj.widget);
      (frame :> GObj.widget)
    in
    dialog#vbox#add txt;
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
end

let _ =
  GtkSignal.user_handler := fun e ->
    Tools.debug "Exception in callback: %s\n%s"
      (Printexc.to_string e)
      (Printexc.get_backtrace ())
