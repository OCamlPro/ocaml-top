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
module OBuf = OcamlBuffer

type top = {
  buffer: GSourceView3.source_buffer;
  mutable process: Top.t option;
  (* marks in the top view where message from different sources are
     printed. Useful for not mixing them, and keeping locations *)
  stdout_mark: GText.mark;
  ocaml_mark: GText.mark;
  prompt_mark: GText.mark;
}


let get_phrases buf (start:GText.iter) (stop:GText.iter) =
  let gbuf = buf.OBuf.gbuffer in
  let bounds = OBuf.get_phrases buf ~start ~stop in
  List.map
    (fun (start,stop) ->
       gbuf#get_text ~start ~stop (),
       OBuf.get_indented_text ~start ~stop buf,
       `MARK (gbuf#create_mark start), `MARK (gbuf#create_mark stop))
    bounds

let region_to_eval buf =
  let gbuf = buf.OBuf.gbuffer in
  let eval_iter = gbuf#get_iter_at_mark buf.OBuf.eval_mark#coerce in
  let eval_iter =
    if gbuf#source_marks_at_iter ~category:"end_block_mark" eval_iter = [] then
      (* the block marks have changed, making the eval_mark invalid.
         Rewind it to the last block mark *)
      let last_phrase = OBuf.last_end_of_phrase buf eval_iter in
      gbuf#move_mark buf.OBuf.eval_mark#coerce ~where:last_phrase;
      last_phrase
    else
      eval_iter
  in
  let point = gbuf#get_iter `INSERT in
  let start, next_point = OBuf.phrase_bounds buf point in
  let last_point =
    if eval_iter#offset < start#offset then eval_iter
    else start
  in
  last_point, next_point

(* Used when there is a new prompt *)
let replace_top_marks top =
  top.buffer#insert ~iter:top.buffer#end_iter "\n";
  top.buffer#move_mark top.stdout_mark
    ~where:top.buffer#end_iter#backward_char;
  top.buffer#insert ~iter:top.buffer#end_iter ~tags:[OBuf.Tags.invisible] " ";
  top.buffer#move_mark top.ocaml_mark
    ~where:top.buffer#end_iter#backward_char;
  top.buffer#move_mark top.prompt_mark
    ~where:top.buffer#end_iter

let duplicate_mark (gbuf:GSourceView3.source_buffer) ?left_gravity mark =
  gbuf#create_mark ?left_gravity (gbuf#get_iter_at_mark mark)

(* Insert in the toplevel view *)
let insert_top ?tags top mark text =
  let iter = top.buffer#get_iter_at_mark mark in
  top.buffer#insert ~iter ?tags text

let display_top_query top phrase =
  insert_top ~tags:[OBuf.Tags.phrase] top top.prompt_mark phrase;
  let phrase_len = String.length phrase in
  if phrase_len > 0 && phrase.[phrase_len - 1] <> '\n' then
    insert_top ~tags:[OBuf.Tags.phrase] top top.prompt_mark "\n"

let display_stdout top response =
  let rec disp_lines = function
    | [] -> ()
    | line::rest ->
        let offset =
          (top.buffer#get_iter_at_mark top.stdout_mark)#line_offset
        in
        let line =
          if offset >= 1024 then ""
          else if offset + String.length line <= 1024 then line
          else (String.sub line 0 (1024 - offset - 3) ^ "...")
        in
        insert_top ~tags:[OBuf.Tags.stdout] top top.stdout_mark line;
        if rest <> [] then
          (insert_top ~tags:[OBuf.Tags.stdout] top top.stdout_mark "\n";
           disp_lines rest)
  in
  disp_lines (Tools.split_lines response)

let display_top_response top response =
  let rec disp_lines = function
    | [] -> ()
    | line::rest ->
        if String.length line > 0 && line.[0] = '#' then
          (if (top.buffer#get_iter_at_mark top.ocaml_mark)#line_offset <> 0
           then insert_top top top.ocaml_mark "\n";
           insert_top top top.ocaml_mark "\n");
        insert_top  (* ~tags:[OBuf.Tags.ocamltop] *) top top.ocaml_mark line;
        if rest <> [] then
          (insert_top top top.ocaml_mark "\n";
           disp_lines rest)
  in
  disp_lines (Tools.split_lines response)

(* Marks characters start_char..end_char within region start_mark..end_mark *)
let mark_error_in_source_buffer buf start_mark end_mark _msg
    start_line start_char end_char =
  let gbuf = buf.OBuf.gbuffer in
  let start_region = gbuf#get_iter_at_mark start_mark in
  let end_region = gbuf#get_iter_at_mark end_mark in
  let min i1 i2 = if i1#offset > i2#offset then i2 else i1 in
  let start =
    min end_region
      ((start_region#forward_lines (start_line - 1))#forward_chars start_char)
  in
  let stop = min end_region (start#forward_chars (end_char - start_char)) in
  let errmark = gbuf#create_source_mark ~category:"error" start in
  (* let tagmark = gbuf#create_mark ~left_gravity:false start in *)
  gbuf#apply_tag OBuf.Tags.error ~start ~stop;
  let mark_remover_id = ref None in
  let callback () =
    (match !mark_remover_id with
     | Some id -> gbuf#misc#disconnect id
     | None -> Tools.debug "Warning, unbound error unmarking callback";
         raise Exit);
    let start = gbuf#get_iter_at_mark errmark#coerce in
    let stop = start#forward_to_tag_toggle (Some OBuf.Tags.error) in
    gbuf#remove_tag OBuf.Tags.error ~start ~stop;
    (* gbuf#remove_source_marks ~category:"error" ~start ~stop ()
     *      (* -- may segfault sometimes (GTK2 or 3) (??!) *) *)
    gbuf#delete_mark errmark#coerce
  in
  mark_remover_id := Some (gbuf#connect#changed ~callback)

(* Messages from the toplevel have already been printed asynchronously by
   [display_top_response]. This function gets the mark where the message was
   printed and the full message, now that we can do more clever stuff on it *)
let handle_response top response response_start_mark
    buf src_start_mark src_end_mark =
  (* returns false on errors, true otherwise *)
  let first_word line =
    let len = String.length line in
    let rec aux i = if i >= len then i else match line.[i] with
        | 'a'..'z' | 'A'..'Z' | '-' | '#' -> aux (i+1)
        | _ -> i
    in
    String.sub line 0 (aux 0)
  in
  let rec split_when acc l f = match l with
    | [] -> List.rev acc, []
    | a::r ->
        if f a then List.rev acc, a::r
        else split_when (a::acc) r f
  in
  let next_msg_line s =
    String.length s = 0 ||
    match s.[0] with
    | ' ' | ')' | '|' | ']' | '}' | '"' -> false
    | _ -> true
  in
  let rec parse_response success iter = function
    | [] -> success
    | line::lines ->
        match first_word line with
        | "val" | "type" | "exception" | "module" | "class"
        | "-" | "Exception" as word
          ->
            let msg, rest = split_when [line] lines next_msg_line in
            let success = success && word <> "Exception" in
            (* Syntax coloration is set by default in the buffer *)
            parse_response success (iter#forward_lines (List.length msg)) rest
        | "Characters" | "File" | "Line" -> (* beginning of an error/warning message *)
            let msg1, rest =
              split_when [line] lines @@ fun line ->
                match first_word line with
                | "Error" | "Warning" -> true
                | _ -> false
            in
            let msg2, rest =
              match rest with
              | hd::tl -> split_when [hd] tl next_msg_line
              | [] -> [], []
            in
            let _ =
              try
                Scanf.sscanf line "Line %d, characters %d-%d:" @@
                mark_error_in_source_buffer buf src_start_mark src_end_mark msg2
              with Scanf.Scan_failure _ | End_of_file ->
                  Tools.debug "OCaml err message parsing failure: %s" line
            in
            let stop =
              iter#forward_lines (List.length msg1 + List.length msg2)
            in
            (* mark in the ocaml buffer *)
            top.buffer#apply_tag OBuf.Tags.ocamltop_err ~start:iter ~stop;
            parse_response false stop rest
        | _ ->
            (* Other messages: override syntax highlighting *)
            let stop = iter#forward_line in
            top.buffer#apply_tag OBuf.Tags.ocamltop ~start:iter ~stop;
            parse_response success stop lines
  in
  let lines = Tools.split_lines response in
  let response_iter = top.buffer#get_iter_at_mark response_start_mark in
  parse_response true response_iter lines

(* Action triggered when the "play" button is pressed *)
let topeval ?(full=false) buf top =
  let gbuf = buf.OBuf.gbuffer in
  let should_update_eval_mark, (start, stop) =
    if full then true, (fst (region_to_eval buf), gbuf#end_iter)
    else if gbuf#has_selection then false, gbuf#selection_bounds
    else true, region_to_eval buf
  in
  if should_update_eval_mark then
    gbuf#move_mark buf.OBuf.eval_mark_end#coerce ~where:stop;
  let cleanup_source_marks phrases =
    List.iter (fun (_,_,start_mark,stop_mark) ->
        gbuf#delete_mark start_mark;
        gbuf#delete_mark stop_mark)
      phrases
  in
  let rec eval_phrases = function
    | [] -> ()
    | (_,_,_,stop_mark) :: _ as phrases
      when should_update_eval_mark &&
           (gbuf#get_iter_at_mark stop_mark)#offset >
           (gbuf#get_iter_at_mark buf.OBuf.eval_mark_end#coerce)#offset
      ->
        (* eval_end has been moved back, meaning the code was just edited *)
        Tools.debug "Stopping evaluation, eval_end was rolled back (%d > %d)"
          (gbuf#get_iter_at_mark stop_mark)#offset
          (gbuf#get_iter_at_mark buf.OBuf.eval_mark_end#coerce)#offset;
        cleanup_source_marks phrases
    | ((""|";;"),_,start_mark,stop_mark) :: rest ->
        gbuf#delete_mark start_mark;
        gbuf#delete_mark stop_mark;
        eval_phrases rest
    | (phrase,indented,start_mark,stop_mark) :: rest as phrases ->
        display_top_query top indented;
        replace_top_marks top;
        let response_start_mark =
          `MARK (duplicate_mark top.buffer top.ocaml_mark)
        in
        match top.process with
        | None -> cleanup_source_marks phrases
        | Some process -> Top.query process phrase @@ fun response ->
            let success =
              handle_response top response response_start_mark
                buf start_mark stop_mark
            in
            top.buffer#delete_mark response_start_mark;
            if (gbuf#get_iter_at_mark stop_mark)#offset >
               (gbuf#get_iter_at_mark buf.OBuf.eval_mark_end#coerce)#offset
            then
              (* stop now if eval_mark_end was rolled back before stop_mark *)
              cleanup_source_marks phrases
            else
              (if should_update_eval_mark then
                 if success then
                   gbuf#move_mark buf.OBuf.eval_mark#coerce ~where:
                     (gbuf#get_iter_at_mark stop_mark)
                 else
                   (let where = gbuf#get_iter_at_mark start_mark in
                    gbuf#move_mark buf.OBuf.eval_mark#coerce ~where;
                    gbuf#move_mark buf.OBuf.eval_mark_end#coerce ~where);
               gbuf#delete_mark start_mark;
               gbuf#delete_mark stop_mark;
               if success then eval_phrases rest
               else cleanup_source_marks rest)
  in
  let phrases = get_phrases buf start stop in
  eval_phrases phrases

let create_buffer () =
  let buffer =
    GSourceView3.source_buffer
      ?language:(OBuf.GSourceView_params.syntax ())
      ?style_scheme:(OBuf.GSourceView_params.style ())
      ~highlight_matching_brackets:false
      ~highlight_syntax:true
      ?undo_manager:None
      ~tag_table:OBuf.Tags.table
      ()
  in
  let create_top_mark () =
    `MARK (buffer#create_mark buffer#end_iter ~left_gravity:false)
  in
  let stdout_mark = create_top_mark () in
  let ocaml_mark  = create_top_mark () in
  let prompt_mark = create_top_mark () in
  { buffer; process = None;
    stdout_mark; ocaml_mark; prompt_mark }

let show_spinner top (view: GSourceView3.source_view) =
  (* This triggers from time to time
Gtk-WARNING **: Allocating size to GtkImage 0x556f667effa0 without calling gtk_widget_get_preferred_width/height(). How does the code know the size to allocate?
     (when scrolling ?) but no idea how to fix it... *)
  let spinner_mark = `MARK (duplicate_mark top.buffer top.prompt_mark) in
  let anim () =
    let (/) = Filename.concat in
    GMisc.image ~file:(!Cfg.datadir / "icons" / "spinner.gif") ()
  in
  fun enable ->
    if enable then
      let iter = top.buffer#get_iter_at_mark top.prompt_mark in
      top.buffer#move_mark spinner_mark ~where:iter;
      ignore @@ top.buffer#create_child_anchor iter;
      let iter = top.buffer#get_iter_at_mark spinner_mark in
      match iter#contents with
      | `CHILD anchor ->
          view#add_child_at_anchor (anim())#coerce anchor
      | _ -> Tools.debug "show_spinner: couldn't find anchor"
    else
      let iter = top.buffer#get_iter_at_mark spinner_mark in
      top.buffer#delete ~start:iter ~stop:iter#forward_char

let rec top_start ~init ~status_change_hook top =
  let schedule f = GMain.Idle.add @@ fun () ->
      try f (); false with e ->
          Printf.eprintf
            "Error in toplevel interaction: %s%!" (Tools.printexc e);
          raise e
  in
  let resp_handler = function
    | Top.Message m -> display_top_response top m
    | Top.User u -> display_stdout top u
    | Top.Exited ->
        init ();
        top.buffer#insert
          ~tags:[OBuf.Tags.ocamltop_warn]
          ~iter:top.buffer#end_iter
          "\t\t*** restarting ocaml ***\n";
  in
  let status_change_hook status =
    status_change_hook status;
    if status = Top.Dead then top_start ~init ~status_change_hook top
  in
  replace_top_marks top;
  init ();
  let process = Top.start schedule resp_handler status_change_hook in
  top.process <- Some process
