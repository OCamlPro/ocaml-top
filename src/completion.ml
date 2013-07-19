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

let is_prefix_char =
  let chars = List.map int_of_char ['.';'_';'\''] in
  fun gchar ->
    Glib.Unichar.isalnum gchar ||
    List.mem gchar chars

let completion_start_iter (iter:GText.iter) : GText.iter =
  let limit = iter#set_line_offset 0 in
  let iter =
    iter#backward_find_char ~limit (fun c -> not (is_prefix_char c))
  in
  if iter#equal limit then iter else iter#forward_char

let completion_end_iter (iter:GText.iter) : GText.iter =
  let limit = iter#forward_to_line_end in
  iter#forward_find_char ~limit (fun c -> not (is_prefix_char c))

let get_completions index buf
    (context: GSourceView2.source_completion_context) =
  let candidates =
    try
      let stop = context#iter in
      let start = completion_start_iter stop in
      let word = buf.OBuf.gbuffer#get_text ~start ~stop () in
      Tools.debug "Completing on %S" word;
      LibIndex.complete index word
    with e ->
        Tools.debug "Exception in completion: %s"
          (Printexc.to_string e);
        []
  in
  List.map (fun info ->
      let text = LibIndex.Print.path info in
      let label = Printf.sprintf "%s %s"
          (LibIndex.Print.name info) (LibIndex.Print.ty info)
      in
      (GSourceView2.source_completion_item ~label ~text
         ?icon:None
         ~info:(LibIndex.Print.ty info)
         ()
       :> GSourceView2.source_completion_proposal)
    ) candidates

let setup_show_type index buf message =
  let gbuf = buf.OBuf.gbuffer in
  let show_type pos =
    let iter = gbuf#get_iter (`OFFSET pos) in
    let start = completion_start_iter iter in
    let stop = completion_end_iter start in
    let msg =
      if stop#offset > start#offset then
        let id = gbuf#get_text ~start ~stop () in
        try
          let i = LibIndex.get index id in
          Tools.debug "Found definition for %s" id;
          let str_ty = LibIndex.Print.ty i in
          let str_ty =
            if str_ty = "" then str_ty else
              LibIndex.(match i.kind with
                  | Type -> " = " ^ str_ty
                  | Exception | Variant _ -> " of " ^ str_ty
                  | Value | Method _ -> ": " ^ str_ty
                  | _ -> str_ty)
          in
          LibIndex.Format.(
            Format.fprintf Format.str_formatter "%a %a%s"
              (fun fmt -> kind fmt) i
              (fun fmt -> path fmt) i
              str_ty
          );
          Format.flush_str_formatter ()
        with Not_found -> ""
      else ""
    in
    let msg = (* Add some nicer-looking utf8 chars *)
      let len = String.length msg in
      let buf = Buffer.create (len + len / 2) in
      let rec replace i =
        if i >= len then Buffer.contents buf
        else if msg.[i] = '-' && msg.[i+1] = '>'
        then (Buffer.add_string buf "\xe2\x86\x92"; replace (i+2))
        else (Buffer.add_char buf msg.[i]; replace (i+1))
      in
      replace 0
    in
    message msg
  in
  let callback =
    let idle_id = ref None in
    fun pos ->
      (match !idle_id with Some id -> GMain.Idle.remove id | None -> ());
      idle_id := Some (GMain.Idle.add @@ fun () -> show_type pos; false)
  in
  ignore @@ gbuf#connect#notify_cursor_position ~callback

let setup_completion index buf (view: GSourceView2.source_view) =
  let ocaml_completion_provider =
    (* ref needed for bootstrap (!) *)
    let provider_ref = ref None in
    let custom_provider : GSourceView2.custom_completion_provider =
      object (self)
        method name = "Available library values"
        method icon =  None
        method populate context =
          let provider =
            match !provider_ref with Some p -> p | None -> assert false
          in
          ignore @@ GMain.Idle.add @@ fun () ->
            let candidates = get_completions index buf context in
            context#add_proposals provider candidates true;
            false
        method matched context = Tools.debug "matched";
          is_prefix_char context#iter#backward_char#char
        method activation = [`USER_REQUESTED] (* ;`INTERACTIVE] *)
        method info_widget _propal = None
        method update_info _propal _info = ()
        method start_iter context _propal iter = false
        method activate_proposal propal iter =
          let pfxlen = iter#offset - (completion_start_iter iter)#offset in
          let text = propal#text in
          let text = String.sub text pfxlen (String.length text - pfxlen) in
          buf.OBuf.gbuffer#begin_user_action ();
          buf.OBuf.gbuffer#insert
            ~iter:(buf.OBuf.gbuffer#get_iter `INSERT)
            text;
          buf.OBuf.gbuffer#end_user_action ();
          true
        method interactive_delay = 2
        method priority = 2
      end
    in
    let provider =
      GSourceView2.source_completion_provider custom_provider
    in
    provider_ref := Some provider;
    provider
  in
  let compl = view#completion in
  compl#set_remember_info_visibility true;
  compl#set_show_headers false;
  ignore (compl#add_provider ocaml_completion_provider);
  (* let compl_visible = ref false in *)
  ignore (view#event#connect#key_press ~callback:(fun ev ->
      if GdkEvent.Key.keyval ev = GdkKeysyms._Tab then
        (Tools.debug "Complete !";
         (* trigger compl; *)
         ignore (compl#show [ocaml_completion_provider]
                   (compl#create_context (buf.OBuf.gbuffer#get_iter `INSERT)));
         true)
      else false
    ));
  Tools.debug "Completion activated"

(* WORK IN PROGRESS: this works but can trigger SEGFAULTS ! *)
let setup buf (view: GSourceView2.source_view) message =
  let index =
    let dirs = [!Cfg.datadir] in
    let dirs =
      try
        let ic = Unix.open_process_in "ocamlc -where" in
        let dirs = (try input_line ic :: dirs with End_of_file -> dirs) in
        ignore (Unix.close_process_in ic);
        dirs
      with Unix.Unix_error _ -> dirs
    in
    LibIndex.load (LibIndex.unique_subdirs dirs)
  in
  if Tools.debug_enabled then (
    (* Enable only for debug at the moment *)
    setup_completion index buf view
  );
  setup_show_type index buf message;
  Tools.debug "Type display activated";
  ()

