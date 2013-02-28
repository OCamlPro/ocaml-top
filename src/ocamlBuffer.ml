open Tools.Ops

module GSourceView_params = struct
  let syntax =
    (GSourceView2.source_language_manager ~default:true)
      #language "objective-caml"
  let style =
    (GSourceView2.source_style_scheme_manager ~default:true)
      #style_scheme "cobalt"
end

type t = {
  mutable filename: string option;
  gbuffer: GSourceView2.source_buffer;
  view: GSourceView2.source_view;
  mutable need_reindent: bool;
  (* mutable error_tags: GText.tag list; *)
  (* mutable indent_tags: GText.tag list; *)
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
    let table = GText.tag_table () in
    table#add phrase#as_tag;
    table#add stdout#as_tag;
    table#add invisible#as_tag;
    table#add error#as_tag;
    table

  let get_indent, indent =
    let indent_tags = Hashtbl.create 64 in
    let reverse = Hashtbl.create 64 in
    (fun t ->
      try Some (Hashtbl.find reverse t#get_oid) with
      | Not_found -> None),
    fun obuf n ->
      try Hashtbl.find indent_tags n with
      | Not_found ->
          let name = Printf.sprintf "indent-%d" n in
          let t = GText.tag ~name () in
          let char_width =
            (obuf.view#misc#pango_context#get_metrics ())#approx_char_width
            / Pango.scale
          in
          Tools.debug "Indent %dx%d => %dpx" n char_width (n*char_width);
          t#set_property (`INDENT (n*char_width));
          Hashtbl.add indent_tags n t;
          Hashtbl.add reverse t#get_oid n;
          table#add t#as_tag;
          t
end

let reindent t =
  let buf = t.gbuffer in
  let reader =
    let text = buf#get_text ~start:buf#start_iter ~stop:buf#end_iter () in
    let pos = ref 0 in
    fun str len ->
      let n = min len (String.length text - !pos) in
      String.blit text !pos str 0 n;
      pos := !pos + n;
      n
  in
  let buf_indent =
    let line = ref 0 in
    fun indent ->
      let start = buf#get_iter (`LINE !line) in
      if start#char = int_of_char ' ' then (
        (* cleanup whitespace (todo: except in comments) *)
        let stop =
          start#forward_find_char
            ~limit:start#forward_to_line_end
            (fun c -> not (Glib.Unichar.isspace c))
        in
        buf#delete ~start ~stop
      );
      let stop = start#forward_to_line_end in
      (* fixme: might leave tags when deleting a line break *)
      List.iter (fun tag -> match Tags.get_indent tag with
        | Some n when n <> indent ->
            buf#remove_tag tag ~start ~stop
        | Some _ | None -> ())
        start#tags;
      buf#apply_tag (Tags.indent t indent) ~start:start#backward_char ~stop;
      incr line
  in
  let input = Nstream.make reader in
  let output = {
    IndentPrinter.
    debug = false;
    config =
      IndentConfig.update_from_string IndentConfig.default "apprentice";
    in_lines = (fun _ -> true);
    indent_empty = true;
    kind = IndentPrinter.Numeric buf_indent;
  }
  in
  IndentPrinter.stream output input

let create ?name ?(contents="")
    (mkview: GSourceView2.source_buffer -> GSourceView2.source_view) =
  let gbuffer =
    if not (Glib.Utf8.validate contents) then
      Tools.recover_error
        "Could not open file %s because it contains invalid utf-8 \
         characters. Please fix it or choose another file"
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
  let t = { filename = name; need_reindent = false; gbuffer; view } in
  ignore @@ gbuffer#connect#modified_changed ~callback:(fun () ->
      Gui.set_window_title "%s%s" (filename_default t) @@
        if gbuffer#modified then "*" else "");
  unmodify t;
  let trigger_reindent () =
    if not t.need_reindent then
      (t.need_reindent <- true;
       ignore @@ GMain.Idle.add @@ fun () ->
         reindent t;
         t.need_reindent <- false;
         false)
  in
  (* TODO: when style changed by sourceview syntax coloration ? *)
  ignore @@ gbuffer#connect#insert_text ~callback:(fun _iter text ->
      let rec contains_space i =
        if i >= String.length text then false
        else match text.[i] with
          | ' ' | '\n' -> true
          | _ -> contains_space (i+1)
      in
      if contains_space 0 then trigger_reindent ()
    );
  ignore @@ gbuffer#connect#delete_range ~callback:(fun ~start:_ ~stop:_ ->
      trigger_reindent ()
    );
  trigger_reindent ();
  gbuffer#end_not_undoable_action ();
  t

let get_selection buf =
  let gbuf = buf.gbuffer in
  if gbuf#has_selection then
    let start,stop = gbuf#selection_bounds in
    Some (gbuf#get_text ~start ~stop ())
  else
    None
