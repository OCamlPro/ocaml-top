open Tools.Ops
module Buffer = OcamlBuffer (* fixme *)

let init_top_view current_buffer_ref toplevel_buffer =
  let top_view = Gui.open_toplevel_view toplevel_buffer in
  (* marks in the top view where message from different sources are
     printed. Useful for not mixing them, and keeping locations *)
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
          if String.length line > 0 && line.[0] = '#' then
            insert_top ocaml_mark "\n";
          insert_top  (* ~tags:[Buffer.Tags.ocamltop] *) ocaml_mark line;
          if rest <> [] then
            (insert_top ocaml_mark "\n";
             disp_lines rest)
    in
    let delim = Str.regexp "\r?\n" in
    let lines = Str.split_delim delim response in
    disp_lines lines
  in
  let mark_error_in_source_buffer (gbuf:GSourceView2.source_buffer)
      ~start ~stop =
    let errmark = gbuf#create_source_mark ~category:"error" start in
    gbuf#apply_tag Buffer.Tags.error ~start ~stop;
    let mark_remover_id = ref None in
    let callback () =
      (match !mark_remover_id with
       | Some id -> gbuf#misc#disconnect id
       | None -> Tools.debug "Warning, unbound error unmarking callback";
           raise Exit);
      let start = gbuf#get_iter_at_mark errmark#coerce in
      let stop = start#forward_to_tag_toggle (Some Buffer.Tags.error) in
      gbuf#remove_tag Buffer.Tags.error ~start ~stop;
      (* buf#remove_source_marks ~category:"error" ~start ~stop ()
           -- may segfault sometimes (??!) *)
      gbuf#delete_mark errmark#coerce
    in
    mark_remover_id := Some (gbuf#connect#changed ~callback);
  in
  (* Messages from the toplevel have already been printed asynchronously by
     [display_top_response]. This function gets the mark where the message was
     printed and the full message, now that we can do more clever stuff on it *)
  let handle_response response response_start_mark
      src_buf src_start_mark src_end_mark =
    (* returns false on errors, true otherwise *)
    let gbuf = src_buf.Buffer.gbuffer in
    (* todo:
       - split message (val/type/etc | error/warning | prompt)
       - colorise each part
       - for errors/warnings, add the marks to the source buffer
    *)
    let lines = Tools.string_split_chars "\r\n" response in
    let response_iter = toplevel_buffer#get_iter_at_mark response_start_mark in
    let first_word line =
      let len = String.length line in
      let rec aux i = if i >= len then i else match line.[i] with
          | 'a'..'z' | 'A'..'Z' | '-' | '#' -> aux (i+1)
          | _ -> i
      in
      String.sub line 0 (aux 0)
    in
    let rec accumulate_until f acc = function
      | [] -> List.rev acc, []
      | a::r ->
          if f a then List.rev acc, a::r
          else accumulate_until f (a::acc) r
    in
    let mark_error start_char end_char =
      Tools.debug "Parsed error from ocaml: chars %d-%d" start_char end_char;
      (* mark in the source buffer *)
      let input_start = gbuf#get_iter_at_mark src_start_mark in
      mark_error_in_source_buffer gbuf
        ~start:(input_start#forward_chars start_char)
        ~stop:(input_start#forward_chars end_char);
    in
    let rec parse_response success iter = function
      | [] -> success
      | line::lines ->
          match first_word line with
          | "val" | "type" | "exception" | "module" | "class"
          | "-" | "Exception" as word
            ->
              let msg, rest =
                accumulate_until
                  (fun s -> String.length s = 0 || s.[0] <> ' ')
                  [line] lines
              in
              let success = success && word <> "Exception" in
              (* Syntax coloration is set by default in the buffer *)
              parse_response success (iter#forward_lines (List.length msg)) rest
          | "Characters" -> (* beginning of an error/warning message *)
              let msg1, rest =
                accumulate_until
                  (fun line -> match first_word line with
                     | "Error" | "Warning" -> true
                     | _ -> false)
                  [line] lines
              in
              let msg2, rest =
                match rest with
                | line::lines ->
                    accumulate_until
                      (fun s -> String.length s = 0 || s.[0] <> ' ')
                      [line] lines
                | [] -> [], []
              in
              let _ =
                try Scanf.sscanf line "Characters %d-%d" mark_error
                with Scanf.Scan_failure _ | End_of_file ->
                    Tools.debug "OCaml err message parsing failure: %s" line
              in
              let stop =
                iter#forward_lines (List.length msg1 + List.length msg2)
              in
              (* mark in the ocaml buffer *)
              toplevel_buffer#apply_tag Buffer.Tags.ocamltop_err
                ~start:iter ~stop;
              parse_response false stop rest
          | _ ->
              (* Other messages: override syntax highlighting *)
              let stop = iter#forward_line in
              toplevel_buffer#apply_tag Buffer.Tags.ocamltop
                ~start:iter ~stop;
              parse_response success stop lines
    in
    parse_response true response_iter lines
  in
  let topeval top =
    let buf = !current_buffer_ref in
    let gbuf = buf.Buffer.gbuffer in
    let rec get_phrases (start:GText.iter) (stop:GText.iter) =
      match start#forward_search ~limit:stop ";;" with
      (* TODO: parse comments/strings to ignore ';;' within strings and
         comments, and check that we don't send an unterminated block to the
         toplevel (after we do that, the only option is to press "Stop") *)
      | Some (a,b) ->
          (gbuf#get_text ~start ~stop:a (),
           Buffer.get_indented_text ~start ~stop:a buf,
           `MARK (gbuf#create_mark start), `MARK (gbuf#create_mark b))
          :: get_phrases b stop
      | None ->
          [gbuf#get_text ~start ~stop (),
           Buffer.get_indented_text ~start ~stop buf,
           `MARK (gbuf#create_mark start), `MARK (gbuf#create_mark stop)]
    in
    let should_update_eval_mark, (start, stop) =
      if gbuf#has_selection then
        false, gbuf#selection_bounds
      else
        true,
        let eval_point = gbuf#get_iter_at_mark buf.Buffer.eval_mark#coerce in
        let point = gbuf#get_iter `INSERT in
        let next_point = match (point#backward_chars 2)#forward_search ";;" with
          | None -> gbuf#end_iter
          | Some (_,b) -> b
        in
        if eval_point#offset < point#offset then
          eval_point, next_point
        else
          let last_point = match point#backward_search ";;" with
          | None -> gbuf#start_iter
          | Some (_,b) -> b
          in
          last_point, next_point
    in
    let rec eval_phrases = function
      | [] -> ()
      | (phrase,indented,start_mark,stop_mark) :: rest ->
          let trimmed = String.trim indented in
          if trimmed = "" then
            (gbuf#delete_mark start_mark;
             gbuf#delete_mark stop_mark;
             eval_phrases rest)
          else
            (display_top_query trimmed;
             replace_marks ();
             let response_start_mark =
               `MARK
                 (toplevel_buffer#create_mark
                    (toplevel_buffer#get_iter_at_mark ocaml_mark))
             in
             Top.query top phrase @@ fun response ->
               let success =
                 handle_response response response_start_mark
                   buf start_mark stop_mark
               in
               toplevel_buffer#delete_mark response_start_mark;
               (* fixme: if the code has been edited in the meantime,
                  we still move the mark... *)
               let start = gbuf#get_iter_at_mark start_mark in
               let stop = gbuf#get_iter_at_mark stop_mark in
               if should_update_eval_mark then
                 gbuf#move_mark
                   buf.Buffer.eval_mark#coerce
                   ~where:(if success then stop else start);
               gbuf#delete_mark start_mark;
               gbuf#delete_mark stop_mark;
               if success then eval_phrases rest)
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
          let buf = !current_buffer_ref in
          buf.Buffer.gbuffer#move_mark
            buf.Buffer.eval_mark#coerce ~where:buf.Buffer.gbuffer#start_iter;
          toplevel_buffer#insert
            ~tags:[Buffer.Tags.ocamltop_warn]
            ~iter:toplevel_buffer#end_iter
            "\t\t*** restarting ocaml ***\n";
    in
    replace_marks ();
    let buf = !current_buffer_ref in
    buf.Buffer.gbuffer#move_mark
      buf.Buffer.eval_mark#coerce ~where:buf.Buffer.gbuffer#start_iter;
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
      toplevel_buffer#delete
        ~start:toplevel_buffer#start_iter
        ~stop:toplevel_buffer#end_iter;
      match !top_ref with
      | Some top ->
          Top.kill top
      | None -> ());
  Gui.Controls.bind `CLEAR (fun () ->
    toplevel_buffer#delete
      ~start:toplevel_buffer#start_iter
      ~stop:toplevel_buffer#end_iter);
  top_ref := Some (top_start ())
