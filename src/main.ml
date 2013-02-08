open Tools.Ops

let set_window_title fmt =
  Printf.ksprintf Gui.main_window#set_title (fmt ^^ " - ocp-edit-simple")

module GSourceView_params = struct
  let syntax =
    (GSourceView2.source_language_manager ~default:true)
      #language "objective-caml"
  let style =
    (GSourceView2.source_style_scheme_manager ~default:true)
      #style_scheme "cobalt"
end

module Buffer = struct
  type t = {
    mutable filename: string option;
    gbuffer: GSourceView2.source_buffer;
    view: GSourceView2.source_view;
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

  module Tags = struct
    let phrase =
      let t = GText.tag ~name:"phrase" () in
      t#set_property (`FOREGROUND "grey80");
      (* t#set_property (`BACKGROUND "black"); *)
      (* property paragraph-background colors entire line, but it
         was introduced in gtk 2.8 and isn't yet in lablgtk... *)
      t#set_property (`INDENT 16); (* fixme: 2*font-width *)
      t

    let stdout =
      let t = GText.tag ~name:"stdout" () in
      t#set_property (`FOREGROUND "yellow");
      t

    let invisible =
      let t = GText.tag ~name:"invisible" () in
      t#set_property (`INVISIBLE true);
      t

    let error =
      let t = GText.tag ~name:"error" () in
      t#set_property (`BACKGROUND "red");
      t

    let table =
      Tools.debug "table";
      let table = GText.tag_table () in
      table#add phrase#as_tag;
      table#add stdout#as_tag;
      table#add invisible#as_tag;
      table#add error#as_tag;
      table
  end

  let create ?name ?(contents="")
      (mkview: GSourceView2.source_buffer -> GSourceView2.source_view) =
    let gbuffer =
      if not (Glib.Utf8.validate contents) then
        Tools.recover_error
          ("Could not open file %s because it contains invalid utf-8 "
           ^^ "characters. Please fix it or choose another file")
          (match name with Some n -> n | None -> "<unnamed>");
      GSourceView2.source_buffer
        ~text:contents
        ?language:GSourceView_params.syntax
        ?style_scheme:GSourceView_params.style
        ~highlight_matching_brackets:true
        ~highlight_syntax:true
        ~tag_table:Tags.table
        ()
    in
    (* workaround: if we don't do this, loading of the file can be undone *)
    gbuffer#begin_not_undoable_action ();
    gbuffer#place_cursor ~where:gbuffer#start_iter;
    let view = mkview gbuffer in
    let t = { filename = name; gbuffer; view } in
    ignore @@ gbuffer#connect#modified_changed ~callback:(fun () ->
      set_window_title "%s%s" (filename_default t) @@
        if gbuffer#modified then "*" else "");
    unmodify t;
    gbuffer#end_not_undoable_action ();
    t

  let get_selection buf =
    let gbuf = buf.gbuffer in
    if gbuf#has_selection then
      let start,stop = gbuf#selection_bounds in
      Some (gbuf#get_text ~start ~stop ())
    else
      None
end

let current_buffer = ref (Buffer.create Gui.open_text_view)
let toplevel_buffer =
  GSourceView2.source_buffer
    ?language:GSourceView_params.syntax
    ?style_scheme:GSourceView_params.style
    ~highlight_matching_brackets:true
    ~highlight_syntax:true
    ?undo_manager:None
    ~tag_table:Buffer.Tags.table
    ()

let rec protect ?(loop=false) f x =
  try
    f x
  with
  | Tools.Recoverable_error message ->
      Gui.Dialogs.error ~title:"Error" message;
      if loop then protect f x
  | exc ->
      Gui.Dialogs.error ~title:"Fatal error"
        (Printf.sprintf"<b>Uncaught exception:</b>\n\n%s"
           (Printexc.to_string exc));
      exit 10

module Actions = struct
  let load_file name =
    protect (Tools.File.load name) @@ fun contents ->
      let buf = Buffer.create ~name ~contents Gui.open_text_view in
      current_buffer := buf

  let confirm_discard k =
    if Buffer.is_modified !current_buffer then
      Gui.Dialogs.confirm ~title:"Please confirm"
        (Printf.sprintf "Discard your changes to %s ?"
         @@ Buffer.filename_default
           ~default:"the current file" !current_buffer)
      @@ k
    else k ()

  let load_dialog () =
    confirm_discard @@ fun () ->
      Gui.Dialogs.choose_file `OPEN load_file

  let save_to_file name () =
    let contents = Buffer.contents !current_buffer in
    protect (Tools.File.save contents name) @@ fun () ->
      Buffer.set_filename !current_buffer name;
      Buffer.unmodify !current_buffer

  let save_to_file_ask ?name () = match name with
    | Some n -> save_to_file n ()
    | None ->
      Gui.Dialogs.choose_file `SAVE  @@ fun name ->
        if Sys.file_exists name then
          Gui.Dialogs.confirm ~title:"Overwrite ?"
            (Printf.sprintf "File %s already exists. Overwrite ?" name)
          @@ save_to_file name
        else
          save_to_file name ()

  let new_empty () =
    confirm_discard @@ fun () ->
      current_buffer := Buffer.create Gui.open_text_view

  let check_before_quit _ =
    Buffer.is_modified !current_buffer &&
      Gui.Dialogs.quit (Buffer.filename !current_buffer) @@ fun () ->
        save_to_file_ask ?name:(Buffer.filename !current_buffer) ();
        Buffer.is_modified !current_buffer
end

let _bind_actions =
  Gui.Controls.bind `NEW Actions.new_empty;
  Gui.Controls.bind `OPEN Actions.load_dialog;
  Gui.Controls.bind `SAVE_AS Actions.save_to_file_ask;
  Gui.Controls.bind `SAVE (fun () ->
    Actions.save_to_file_ask ?name:(Buffer.filename !current_buffer) ());
  Gui.Controls.bind `QUIT (fun () ->
    if not (Actions.check_before_quit ()) then Gui.main_window#destroy ())

let init_top_view () =
  let top_view = Gui.open_toplevel_view toplevel_buffer in
  let stdout_mark, ocaml_mark, prompt_mark =
    let create_top_mark () =
      `MARK (toplevel_buffer#create_mark
          toplevel_buffer#end_iter
          ~left_gravity:false)
    in
    create_top_mark (), create_top_mark (), create_top_mark ()
  in
  ignore @@ toplevel_buffer#connect#changed ~callback:(fun () ->
      ignore @@ top_view#scroll_mark_onscreen prompt_mark);
  let replace_marks () =
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n";
    toplevel_buffer#move_mark stdout_mark
      ~where:toplevel_buffer#end_iter#backward_char;
    toplevel_buffer#insert ~iter:toplevel_buffer#end_iter
      ~tags:[Buffer.Tags.invisible] " ";
    toplevel_buffer#move_mark ocaml_mark
      ~where:toplevel_buffer#end_iter#backward_char;
    toplevel_buffer#move_mark prompt_mark
      ~where:toplevel_buffer#end_iter;
  in
  let insert_top ?tags mark text =
    let iter = toplevel_buffer#get_iter_at_mark mark in
    toplevel_buffer#insert ~iter ?tags text
  in
  let display_top_query phrase =
    (* toplevel_buffer#insert ~iter:toplevel_buffer#end_iter "\n# "; *)
    insert_top ~tags:[Buffer.Tags.phrase] prompt_mark phrase;
    let phrase_len = String.length phrase in
    if phrase_len > 0 && phrase.[phrase_len - 1] <> '\n' then
      insert_top ~tags:[Buffer.Tags.phrase] prompt_mark "\n"
  in
  let display_stdout response =
      let rec disp_lines = function
        | [] -> ()
        | line::rest ->
            let offset =
              (toplevel_buffer#get_iter_at_mark stdout_mark)#line_offset
            in
            let line =
              if offset >= 1024 then ""
              else if offset + String.length line <= 1024 then line
              else
                let s = String.sub line 0 (1024 - offset) in
                String.blit "..." 0 s (1024 - offset - 3) 3;
                s
            in
            insert_top ~tags:[Buffer.Tags.stdout] stdout_mark line;
            if rest <> [] then
              (insert_top ~tags:[Buffer.Tags.stdout] stdout_mark "\n";
               disp_lines rest)
      in
      let delim = Str.regexp "\r?\n" in
      let lines = Str.split_delim delim response in
      disp_lines lines
  in
  let display_top_response response =
    let rec disp_lines = function
      | [] -> ()
      | line::rest ->
          if String.length line > 512 then failwith "topfail";
          insert_top ocaml_mark line;
          if rest <> [] then
            (insert_top ocaml_mark "\n";
             disp_lines rest)
    in
    let delim = Str.regexp "\r?\n" in
    let lines = Str.split_delim delim response in
    disp_lines lines
  in
  let handle_response response buf start_mark end_mark =
    (* TODO: parse and handle error messages *)
    Tools.debug "Was supposed to parse and analyse %S" response;
    let error_regex =
      Str.regexp "^Characters \\([0-9]+\\)-\\([0-9]+\\)"
    in
    try
      let _i = Str.search_forward error_regex response 0 in
      let start_char, end_char =
        int_of_string @@ Str.matched_group 1 response,
        int_of_string @@ Str.matched_group 2 response
      in
      Tools.debug "Parsed error from ocaml: chars %d-%d" start_char end_char;
      buf#apply_tag Buffer.Tags.error
        ~start:((buf#get_iter_at_mark start_mark)#forward_chars start_char)
        ~stop: ((buf#get_iter_at_mark start_mark)#forward_chars end_char)
    with Not_found -> ()
  in
  let topeval top =
    let buf = !current_buffer.Buffer.gbuffer in
    let rec get_phrases (start:GText.iter) (stop:GText.iter) =
      match start#forward_search ~limit:stop ";;" with
      | Some (a,b) ->
          (buf#get_text ~start ~stop:a (),
           `MARK (buf#create_mark start), `MARK (buf#create_mark a))
          :: get_phrases b stop
      | None ->
          [buf#get_text ~start ~stop (),
           `MARK (buf#create_mark start), `MARK (buf#create_mark stop)]
    in
    let rec eval_phrases = function
      | [] -> ()
      | (phrase,start_mark,stop_mark) :: rest ->
          let trimmed = String.trim phrase in
          if trimmed = "" then
            (buf#delete_mark start_mark;
             buf#delete_mark stop_mark;
             eval_phrases rest)
          else
            (display_top_query trimmed;
             replace_marks ();
             Top.query top phrase @@ fun response ->
               handle_response response buf start_mark stop_mark;
               buf#delete_mark start_mark; (* really, not the Gc's job ? *)
               buf#delete_mark stop_mark;
               eval_phrases rest)
    in
    let start, stop =
      if buf#has_selection then buf#selection_bounds
      else buf#start_iter, buf#end_iter
    in
    let phrases = get_phrases start stop in
    eval_phrases phrases
  in
  let top_ref = ref None in
  let rec top_start () =
    let schedule f = GMain.Idle.add @@ fun () ->
        try f (); false with
          e ->
            Printf.eprintf "Error in toplevel interaction: %s%!"
              (Tools.printexc e);
            raise e
    in
    let resp_handler = function
      | Top.Message m -> display_top_response m
      | Top.User u -> display_stdout u
      | Top.Exited ->
          toplevel_buffer#insert ~iter:toplevel_buffer#end_iter
            "\t\t*** restarting ocaml ***\n";
    in
    replace_marks ();
    Top.start schedule resp_handler status_change_hook
  and status_change_hook = function
    | Top.Dead ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.disable `STOP;
        top_ref := Some (top_start ())
    | Top.Ready ->
        Gui.Controls.enable `EXECUTE;
        Gui.Controls.disable `STOP
    | Top.Busy _ ->
        Gui.Controls.disable `EXECUTE;
        Gui.Controls.enable `STOP
  in
  Gui.Controls.bind `EXECUTE (fun () ->
    topeval @@ match !top_ref with
    | Some top -> top
    | None -> let top = top_start () in
        top_ref := Some top; top);
  Gui.Controls.bind `STOP (fun () ->
    match !top_ref with
    | Some top -> Top.stop top
    | None -> ());
  Gui.Controls.bind `RESTART (fun () ->
    match !top_ref with
    | Some top -> Top.kill top
    | None -> ());
  Gui.Controls.bind `CLEAR (fun () ->
    toplevel_buffer#delete
      ~start:toplevel_buffer#start_iter
      ~stop:toplevel_buffer#end_iter);
  top_ref := Some (top_start ())

let _ =
  Tools.debug "Init done, showing main window";
  if Array.length Sys.argv > 1 then Actions.load_file Sys.argv.(1);
  init_top_view ();
  Gui.main_window#show();
  protect ~loop:true GMain.main ()
