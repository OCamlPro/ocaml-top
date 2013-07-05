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

module GSourceView_params = struct
  let syntax () =
    let syntax_mgr = GSourceView2.source_language_manager ~default:true in
    if syntax_mgr#search_path <> [!Cfg.datadir] then
      syntax_mgr#set_search_path [!Cfg.datadir];
    let syn = syntax_mgr#language "ocp-edit-ocaml" in
    if syn = None then Tools.debug "WARNING: ocaml language def not found";
    syn

  let style () =
    let style_mgr = GSourceView2.source_style_scheme_manager ~default:true in
    if style_mgr#search_path <> [!Cfg.datadir] then
      style_mgr#set_search_path [!Cfg.datadir];
    let sty = style_mgr#style_scheme "cobalt" in
    if sty = None then Tools.debug "WARNING: style def not found";
    sty
end

type reindent_needed = No_reindent
                     | Reindent_delayed of int
                     | Reindent_line of int
                     | Reindent_after of int
                     | Reindent_full

type t = {
  mutable filename: string option;
  gbuffer: GSourceView2.source_buffer;
  mutable need_reindent: reindent_needed;
  eval_mark: GSourceView2.source_mark;
  eval_mark_end: GSourceView2.source_mark;
  (* ordered from bottom to top *)
  mutable block_marks: GSourceView2.source_mark list;
  mutable on_reindent: unit -> unit;
}

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
    t#set_property (`FOREGROUND "grey60");
    (* t#set_property (`BACKGROUND "black"); *)
    (* property paragraph-background colors entire line, but it
       was introduced in gtk 2.8 and isn't yet in lablgtk... *)
    t#set_property (`INDENT (2 * !Cfg.char_width));
    t

  let stdout =
    let t = GText.tag ~name:"stdout" () in
    t#set_property (`FOREGROUND "#dd0");
    t

  let ocamltop =
    let t = GText.tag ~name:"ocamltop" () in
    t#set_property (`FOREGROUND "white");
    t

  let ocamltop_err =
    let t = GText.tag ~name:"ocamltop_err" () in
    t#set_property (`FOREGROUND "#e44");
    t

  let ocamltop_warn =
    let t = GText.tag ~name:"ocamltop_warn" () in
    t#set_property (`FOREGROUND "orange");
    t

  let invisible =
    let t = GText.tag ~name:"invisible" () in
    t#set_property (`INVISIBLE true);
    t

  let error =
    let t = GText.tag ~name:"error" () in
    t#set_property (`UNDERLINE `SINGLE); (* BACKGROUND "red" *)
    t

  let table =
    let table = GText.tag_table () in
    table#add phrase#as_tag;
    table#add stdout#as_tag;
    table#add ocamltop#as_tag;
    table#add ocamltop_err#as_tag;
    table#add ocamltop_warn#as_tag;
    table#add invisible#as_tag;
    table#add error#as_tag;
    table

  let get_indent, indent =
    let indent_tags = Hashtbl.create 64 in
    let reverse = Hashtbl.create 64 in
    let update_if_font_changed =
      let char_width = ref !Cfg.char_width in
      fun () ->
        if !char_width <> !Cfg.char_width then
          (char_width := !Cfg.char_width;
           phrase#set_property (`INDENT (2 * !char_width));
           Hashtbl.iter (fun n t -> t#set_property (`INDENT (n * !char_width)))
             indent_tags)
    in
    (fun t ->
      try Some (Hashtbl.find reverse t#get_oid) with
      | Not_found -> None),
    fun obuf n ->
      update_if_font_changed ();
      try Hashtbl.find indent_tags n with
      | Not_found ->
          let name = Printf.sprintf "indent-%d" n in
          let t = GText.tag ~name () in
          t#set_property (`INDENT (n * !Cfg.char_width));
          Hashtbl.add indent_tags n t;
          Hashtbl.add reverse t#get_oid n;
          table#add t#as_tag;
          t
end

let skip_space_backwards buf ?(limit=buf.gbuffer#start_iter) (iter:GText.iter) =
  let limit = limit#backward_char in
  let it =
    iter#backward_find_char ~limit (fun c -> not (Glib.Unichar.isspace c))
  in
  it#forward_char

let skip_space_forwards buf ?limit (iter:GText.iter) =
  iter#forward_find_char ?limit (fun c -> not (Glib.Unichar.isspace c))


let next_beg_of_phrase buf (iter: GText.iter) =
  (* we got buf.gbuffer#forward/backward_iter_to_source_mark, but it uses
     side-effects on the iters, aughh. *)
  let offset = iter#offset in
  List.fold_left
    (fun acc mark ->
       let it = buf.gbuffer#get_iter_at_mark mark#coerce in
       if it#offset < acc#offset && it#offset > offset then it else acc)
    buf.gbuffer#end_iter
    buf.block_marks

let last_beg_of_phrase buf
    ?(default  = fun buf -> buf.gbuffer#start_iter)
    (iter: GText.iter) =
  let offset = iter#offset in
  let it_opt =
    List.fold_left
      (fun acc mark ->
         let it = buf.gbuffer#get_iter_at_mark mark#coerce in
         if acc = None && it#offset <= offset then Some it else acc)
      None
      buf.block_marks
  in
  match it_opt with Some it -> it | None -> default buf

let first_beg_of_phrase buf =
  let rec last = function [x] -> x | x::y -> last y | [] -> raise Not_found in
  try buf.gbuffer#get_iter_at_mark (last buf.block_marks)#coerce
  with Not_found -> buf.gbuffer#start_iter

let reindent_line n = Reindent_line n
let reindent_after n = Reindent_after n
let reindent_full = Reindent_full

let reindent_max r1 r2 = match r1, r2 with
  | No_reindent, r | r, No_reindent -> r
  | Reindent_full, _ | _, Reindent_full -> Reindent_full
  | Reindent_delayed l1, Reindent_delayed l2 when l1 = l2 -> Reindent_delayed l1
  | (Reindent_line l1 | Reindent_delayed l1),
    (Reindent_line l2 | Reindent_delayed l2)
    when l1 = l2
    -> Reindent_line l1
  | (Reindent_line l1 | Reindent_after l1 | Reindent_delayed l1),
    (Reindent_line l2 | Reindent_after l2 | Reindent_delayed l2)
    -> Reindent_after (min l1 l2)

let reindent t =
  let buf = t.gbuffer in
  (* ensure buffer ends with a newline *)
  if buf#end_iter#line_offset > 0 then (
    let cursor = buf#create_mark (buf#get_iter_at_mark `INSERT) in
    buf#insert ~iter:buf#end_iter "\n";
    buf#place_cursor ~where:(buf#get_iter_at_mark (`MARK cursor))
  );
  let in_lines = match t.need_reindent with
    | No_reindent | Reindent_delayed _ -> assert false
    (* l is a gtk line, starting at 0. n is an ocp-indent line starting at 1 *)
    | Reindent_line l -> (fun n -> n = l + 1)
    | Reindent_after l -> (fun n -> n >= l + 1)
    | Reindent_full -> (fun _ -> true)
  in
  let buf_indent block elt (last,line,col,block_marks) =
    match elt with
    | IndentPrinter.Whitespace w ->
      last, line, col + String.length w, block_marks
    | IndentPrinter.Text txt ->
      if IndentBlock.is_in_comment block then
        last, line, col + String.length txt, block_marks
      else
        let block_marks =
          if IndentBlock.is_at_top block && block <> IndentBlock.empty ||
             last = ";;"
          then
            buf#create_source_mark
              ~name:("block."^string_of_int (List.length block_marks))
              ~category:"block_mark"
              ((buf#get_iter (`LINE (line - 1)))#forward_chars col)
            :: block_marks
          else block_marks
        in
        txt, line, col + String.length txt, block_marks
    | IndentPrinter.Newline ->
      last, line + 1, 0, block_marks
    | IndentPrinter.Indent indent ->
      if in_lines line then
        let start = buf#get_iter (`LINE (line - 1)) in
        if start#char = int_of_char ' ' then (
          (* cleanup whitespace (todo: except in comments) *)
          let stop =
            start#forward_find_char
              ~limit:start#forward_to_line_end
              (fun c -> not (Glib.Unichar.isspace c))
          in
          buf#delete ~start ~stop
        );
        let stop =
          if start#ends_line then start#forward_char
          else start#forward_to_line_end
        in
        (* fixme: might leave tags when deleting a line break *)
        List.iter (fun tag -> match Tags.get_indent tag with
            | Some n when n <> indent ->
              buf#remove_tag tag ~start ~stop
            | Some _ | None -> ())
          start#tags;
        buf#apply_tag (Tags.indent t indent) ~start:start#backward_char ~stop;
        last, line, col, block_marks
      else
        last, line, col, block_marks
  in
  let input =
    Nstream.of_string
      (buf#get_text ~start:buf#start_iter ~stop:buf#end_iter ())
  in
  let output = {
    IndentPrinter.
    debug = false;
    config =
      IndentConfig.update_from_string IndentConfig.default "apprentice";
    in_lines;
    indent_empty = true;
    adaptive = false;
    kind = IndentPrinter.Extended buf_indent;
  }
  in
  (* Don't use remove_source_marks, it can segfault *)
  let _ = List.iter (fun mark -> buf#delete_mark mark#coerce) t.block_marks in
  let block_marks = []
    (* [buf#create_source_mark ~name:("block.0") ~category:"block_mark" (buf#start_iter)] *)
  in
  let _last, _line, _col, block_marks =
    IndentPrinter.proceed output input IndentBlock.empty (";;",1,0,block_marks)
  in
  t.block_marks <- block_marks

let trigger_reindent ?cont t reindent_needed =
  (match cont with
   | None -> ()
   | Some ct -> t.on_reindent <- fun () -> t.on_reindent (); ct ());
  match t.need_reindent with
  | No_reindent | Reindent_delayed _ ->
    t.need_reindent <- reindent_max t.need_reindent reindent_needed;
    ignore @@ GMain.Idle.add @@ fun () ->
      ignore @@ reindent t;
      t.need_reindent <-
        (match reindent_needed with
         | Reindent_line l -> Reindent_delayed l
         | _ -> No_reindent);
      let ct = t.on_reindent in
      t.on_reindent <- (fun () -> ());
      ct ();
      false
  | current ->
    t.need_reindent <- reindent_max current reindent_needed

let raw_contents buf = buf.gbuffer#get_text ()

let get_indented_text ~start ~stop buf =
  let indent_at it =
    List.fold_left (fun indent tag ->
        match Tags.get_indent tag with
        | None -> indent
        | Some n -> n
    ) 0 it#tags
  in
  let out = Buffer.create 256 in
  let rec get_lines start =
    if start#offset >= stop#offset then ()
    else
      let stop =
        let s =
          if start#ends_line then start#forward_char
          else start#forward_line
        in
        if s#offset >= stop#offset then stop else s
      in
      if not start#ends_line then
        for i = 1 to indent_at start do Buffer.add_char out ' ' done;
      Buffer.add_string out (buf.gbuffer#get_text ~start ~stop ());
      get_lines stop
  in
  get_lines start;
  Buffer.contents out

(* re-indented contents *)
let contents buf =
  get_indented_text
    ~start:buf.gbuffer#start_iter ~stop:buf.gbuffer#end_iter
    buf

let create ?name ?(contents="") () =
  let gbuffer =
    if not (Glib.Utf8.validate contents) then
      Tools.recover_error
        "Could not open %s because it contains invalid utf-8 \
         characters. Please fix it or choose another file"
        (match name with Some n -> n | None -> "this file");
    GSourceView2.source_buffer
      ~text:contents
      ?language:(GSourceView_params.syntax ())
      ?style_scheme:(GSourceView_params.style ())
      ~highlight_matching_brackets:true
      ~highlight_syntax:true
      ~tag_table:Tags.table
      ()
  in
  (* workaround: if we don't do this, loading of the file can be undone *)
  gbuffer#begin_not_undoable_action ();
  gbuffer#place_cursor ~where:gbuffer#start_iter;
  let eval_mark =
    gbuffer#create_source_mark ~name:"eval" ~category:"eval" gbuffer#start_iter
  in
  let eval_mark_end =
    gbuffer#create_source_mark ~name:"eval_next" ~category:"eval_next"
      gbuffer#start_iter
  in
  gbuffer#end_not_undoable_action ();
  { filename = name;
    need_reindent = Reindent_full;
    gbuffer;
    eval_mark; eval_mark_end;
    block_marks = [];
    on_reindent = (fun () -> ()) }

let setup_indent buf =
  let gbuf = buf.gbuffer in
  (* reindent is triggered on input containing special characters *)
  let rec contains_sp text i =
    if i >= String.length text then false
    else match text.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '\'' | '`' |'_' ->
        contains_sp text (i+1)
      | _ -> true
  in
  ignore @@ gbuf#connect#insert_text ~callback:(fun iter text ->
      if contains_sp text 0 then
        trigger_reindent buf (if String.contains text '\n'
                              then Reindent_after iter#line
                              else Reindent_line iter#line)
      else
        buf.need_reindent <- reindent_max buf.need_reindent
            (Reindent_delayed iter#line)
    );
  ignore @@ gbuf#connect#notify_cursor_position ~callback:(fun pos ->
      match buf.need_reindent with
      | Reindent_delayed l | Reindent_line l ->
        if (gbuf#get_iter (`OFFSET pos))#line <> l then
          trigger_reindent buf (Reindent_after l)
      | _ -> ()
    );
  ignore @@ reindent buf;
  buf.need_reindent <- No_reindent;
  unmodify buf;
  ignore @@ gbuf#connect#delete_range ~callback:(fun ~start ~stop ->
      let line = start#line in
      trigger_reindent buf (if line = stop#line then Reindent_line line
                            else Reindent_after line)
    );
  ignore @@ gbuf#connect#changed
      ~callback:(fun () ->
          let it = gbuf#get_iter `INSERT in
          let replace_before_cursor mark =
            let iter = gbuf#get_iter_at_mark mark#coerce in
            if it#offset < iter#offset then
              gbuf#move_mark mark#coerce ~where:(last_beg_of_phrase buf it)
          in
          replace_before_cursor buf.eval_mark;
          replace_before_cursor buf.eval_mark_end)
  (* TODO:
     - forbid pasting of styled text from the top to the text buffer
     (discard style) ; lablgtk doesn't seem to bind the functions needed to do
     this :/
  *)

let get_selection buf =
  let gbuf = buf.gbuffer in
  if gbuf#has_selection then
    let start,stop = gbuf#selection_bounds in
    Some (get_indented_text buf ~start ~stop)
  else
    None
