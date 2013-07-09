(**************************************************************************)
(*                                                                        *)
(*  Copyright 2013 OCamlPro                                               *)
(*                                                                        *)
(*  All rights reserved.  This file is distributed under the terms of     *)
(*  the GNU Public License version 3.0.                                   *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(**************************************************************************)

open Tools.Ops

let _ = GtkMain.Main.init()

module Controls = struct
  type t = [ `NEW | `OPEN | `SAVE | `SAVE_AS
           | `EXECUTE | `EXECUTE_ALL | `STOP | `RESTART | `CLEAR
           | `PREFERENCES | `ZOOM_IN | `ZOOM_OUT | `QUIT ]

  let stock (*: t -> GtkStock.id*) = function
    | `RESTART -> `REFRESH
    | `EXECUTE_ALL -> `MEDIA_FORWARD
    | #GtkStock.id as id -> id

  let icon t =
    let name = match t with
      | `NEW -> "new"
      | `OPEN -> "open"
      | `SAVE -> "save"
      | `SAVE_AS -> "save-as"
      | `EXECUTE -> "execute"
      | `EXECUTE_ALL -> "execute-all"
      | `STOP -> "stop"
      | `RESTART -> "restart"
      | `CLEAR -> "clear"
      | `PREFERENCES -> "setup"
      | `ZOOM_IN -> "zoom-in"
      | `ZOOM_OUT -> "zoom-out"
      | `QUIT -> "quit"
    in
    let file =
      let (/) = Filename.concat in
      !Cfg.datadir / "icons" / name ^ ".png"
    in
    let pixbuf = GdkPixbuf.from_file_at_size file ~width:22 ~height:22 in
    let img = GMisc.image ~pixbuf () in
    img

  let to_string command = GtkStock.convert_id (stock command)

  let help: t -> string * string = function
    | `NEW -> "New","Create a new file"
    | `OPEN -> "Open...","Select an existing file to edit"
    | `SAVE -> "Save","Save the current file"
    | `SAVE_AS -> "Save as...","Select a file to save the current program to"
    | `EXECUTE -> "Run","Run the current program up to the cursor, \
                         or the selection if any"
    | `EXECUTE_ALL -> "Run to end",
                      "Run the current program as far as possible"
    | `STOP -> "Stop","Stop ongoing program execution"
    | `RESTART -> "Restart","Terminate the current toplevel and start a new one"
    | `CLEAR -> "Clear","Clear the toplevel window history"
    | `PREFERENCES -> "Setup","Configuration options"
    | `ZOOM_IN -> "Zoom in","Make the font bigger"
    | `ZOOM_OUT -> "Zoom out","Make the font smaller"
    | `QUIT -> "Quit","Quit ocaml-top"

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
  ([`CONTROL], GdkKeysyms._plus),   `ZOOM_IN;
  ([`CONTROL], GdkKeysyms._minus),  `ZOOM_OUT;
  ([`CONTROL], GdkKeysyms._q),      `QUIT;
]

let add objs container =
  List.iter (fun o -> container#add (o :> GObj.widget)) objs;
  container

let pack objs (container: GPack.box) =
  List.iter (fun o -> container#pack o) objs;
  container

let as_widget o = (o :> GObj.widget)

let main_window () =
  let logo = GdkPixbuf.from_file (Filename.concat !Cfg.datadir "logo.png") in
  let tooltips = GData.tooltips () in
  let mkbutton ctrl =
    let label,text = Controls.help ctrl in
    let btn = GButton.tool_button ~stock:(Controls.stock ctrl) ~label () in
    Controls.add_trigger ctrl btn;
    btn#set_label label;
    btn#set_icon_widget (Controls.icon ctrl :> GObj.widget);
    tooltips#set_tip ~text (btn :> GObj.widget);
    (* ignore @@ btn#connect#clicked ~callback:(fun () -> Controls.trigger ctrl); *)
    (btn :> GObj.widget)
  in
  let win =
    GWindow.window
      ~title:"ocaml-top"
      ~height:600 ~allow_shrink:true (* ~width:800 ~show:true *)
      ~icon:logo
      ()
    |> add [
      GPack.vbox ()
      |> pack [
        GButton.toolbar ~style:`BOTH ()
        |> add [
          mkbutton `NEW;
          mkbutton `OPEN;
          mkbutton `SAVE;
          mkbutton `SAVE_AS;
          (GButton.separator_tool_item () :> GObj.widget);
          mkbutton `EXECUTE;
          mkbutton `STOP;
          mkbutton `RESTART;
          mkbutton `EXECUTE_ALL;
          (* mkbutton `CLEAR; *)
          (GButton.tool_item ~expand:true () :> GObj.widget);
          mkbutton `QUIT;
        ]
        |> as_widget;
      ]
      |> add [
        GPack.paned `HORIZONTAL ()
        |> add [
          main_view;
          toplevel_view;
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
    let keyval = GdkEvent.Key.keyval ev in
    match
      List.filter
        (fun ((st,kv),_) ->
           keyval = kv && List.for_all (fun k -> List.mem k state) st)
        shortcuts
    with
    | (_,action)::_ -> Controls.trigger action; false
    | [] -> false);
  win

let set_window_title window fmt =
  Printf.ksprintf window#set_title (fmt ^^ " - ocaml-top")

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
  let _set_mark_categories =
    let (/) = Filename.concat in
    let icon name = GdkPixbuf.from_file (!Cfg.datadir/"icons"/name^".png") in
    view#set_mark_category_pixbuf ~category:"block_mark"
      (Some (icon "block_marker"));
    view#set_mark_category_pixbuf ~category:"eval_next"
      (if Tools.debug_enabled then Some (icon "eval_marker_next")
       else None);
    view#set_mark_category_pixbuf ~category:"eval"
      (Some (icon "eval_marker"));
    view#set_mark_category_pixbuf ~category:"error"
      (Some (icon "err_marker"));
    view#set_mark_category_priority ~category:"block_mark" 1;
    view#set_mark_category_priority ~category:"eval_next" 3;
    view#set_mark_category_priority ~category:"eval" 4;
    view#set_mark_category_priority ~category:"error" 5;
  in
  view#misc#modify_font_by_name !Cfg.font;
  main_view#add (view :> GObj.widget);
  view#misc#set_size_request ~width:672 ();
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
  view#misc#modify_font_by_name !Cfg.font;
  toplevel_view#add (view :> GObj.widget);
  view#misc#set_size_request ~width:578 ();
  view

let set_font str =
  let font = new GPango.font_description (GPango.font_description str) in
  let set_view (view: GObj.widget) =
    view#misc#modify_font font#fd;
    Cfg.char_width :=
      GPango.to_pixels
        (view#misc#pango_context#get_metrics ())#approx_char_width
  in
  Cfg.font := font#to_string;
  List.iter set_view main_view#children;
  List.iter set_view toplevel_view#children;
  Tools.debug "Font set: %S (char width: %d)"
    font#to_string !Cfg.char_width


module Dialogs = struct

  (* Return type of a function that would return 'a, but is in CPS form *)
  type 'a cps = ('a -> unit) -> unit

  let choose_file action ?(cancel = fun () -> ()) k =
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
      | `CANCEL | `DELETE_EVENT -> dialog#destroy (); cancel ()
      | `APPLY ->
        match dialog#filename with
        | None -> dialog#destroy (); cancel ()
        | Some name -> dialog#destroy (); name |> k
    in
    dialog#add_filter @@
      GFile.filter ~name:"OCaml source (*.ml)" ~patterns:["*.ml";"*.ml?"] ();
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

  let quit filename ~save ~quit =
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
    ignore @@ dialog#connect#response ~callback:(function
      | `SAVE -> dialog#destroy (); save () @@ quit
      | `QUIT -> dialog#destroy (); quit ()
      | `CANCEL | `DELETE_EVENT -> dialog#destroy ());
    dialog#show ()

  let confirm ~title message ?(no = fun () -> ()) k =
    let dialog = GWindow.message_dialog
      ~title
      ~message
      ~use_markup:true
      ~message_type:`QUESTION
      ~buttons:GWindow.Buttons.yes_no
      ()
    in
    ignore @@ dialog#connect#response ~callback:(function
        | `YES -> dialog#destroy () |> k
        | `NO | `DELETE_EVENT -> dialog#destroy () |> no);
    dialog#show ()

  let preferences ~on_font_change () =
    let dialog = GWindow.font_selection_dialog () in
    ignore @@ dialog#connect#response ~callback:(function
      | `APPLY ->
          set_font dialog#selection#font_name;
          on_font_change ()
      | `OK ->
          set_font dialog#selection#font_name;
          dialog#destroy ();
          on_font_change ()
      | `CANCEL | `DELETE_EVENT -> dialog#destroy ());
    dialog#show ()
end
