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

module OBuf = OcamlBuffer

(* WORK IN PROGRESS: this works but can trigger SEGFAULTS ! *)
let setup buf (view: GSourceView2.source_view) =
  let lib_index =
    (* temporary *)
    let rec subdirs acc path =
      Array.fold_left
        (fun acc p ->
          let path = Filename.concat path p in
          if Sys.is_directory path then subdirs acc path else acc)
        (path::acc)
        (Sys.readdir path)
    in
    let ocaml_dirs =
      let ic = Unix.open_process_in "ocamlc -where" in
      let dir = input_line ic in
      ignore (Unix.close_process_in ic);
      subdirs [] dir
    in
    LibIndex.load ocaml_dirs
  in
  let ocaml_completion_provider =
    (* ref needed for bootstrap (!) *)
    let provider_ref = ref None in
    let is_prefix_char =
      let chars = List.map int_of_char ['.';'_';'\''] in
      fun gchar ->
        Glib.Unichar.isalnum gchar ||
        List.mem gchar chars
    in
    let completion_start_iter (iter:GText.iter) : GText.iter =
      let limit = iter#set_line_offset 0 in
      let iter =
        iter#backward_find_char ~limit (fun c -> not (is_prefix_char c))
      in
      if iter#equal limit then iter else iter#forward_char
    in
    let custom_provider : GSourceView2.custom_completion_provider =
      object (self)
        method name = Tools.debug "name";
          "Available library values"
        method icon =  Tools.debug "icon"; None
        method populate : GSourceView2.source_completion_context -> unit =
          fun context ->
            Tools.debug "populate";
            let candidates =
              let stop = context#iter in
              let start = completion_start_iter stop in
              let word = buf.OBuf.gbuffer#get_text ~start ~stop () in
              Tools.debug "Completing on %S" word;
              LibIndex.complete lib_index word
            in
            let propals =
              List.map (fun info ->
                  (GSourceView2.source_completion_item
                     ~label:(LibIndex.Print.name info)
                     ~text:(LibIndex.Print.path info)
                     ?icon:None
                     ~info:(LibIndex.Print.ty info)
                     ()
                   :> GSourceView2.source_completion_proposal)
                ) candidates
            in
            context#add_proposals
              (match !provider_ref with Some p -> p | None -> assert false)
              propals
              true;
        method matched context = Tools.debug "matched";
          is_prefix_char context#iter#backward_char#char
        method activation = [`USER_REQUESTED] (* `INTERACTIVE *)
        method info_widget _propal = None
        method update_info _propal _info = ()
        method start_iter context _propal iter =
          let start =
            completion_start_iter (buf.OBuf.gbuffer#get_iter `INSERT)
          in
          (* ouch, answers by side effect on iter... *)
          iter#nocopy#assign start#nocopy;
          true
        method activate_proposal propal iter =
          let pfxlen = iter#offset - (completion_start_iter iter)#offset in
          let text = propal#text in
          let text = String.sub text pfxlen (String.length text - pfxlen) in
          buf.OBuf.gbuffer#insert
            ~iter:(buf.OBuf.gbuffer#get_iter `INSERT)
            text;
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
  Tools.debug "Completion activated";
  ()

