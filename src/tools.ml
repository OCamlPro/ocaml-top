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

let debug_enabled =
  try match Sys.getenv "OCP_DEBUG" with "" | "0" -> false | _ -> true
  with Not_found -> false

let debug =
  if debug_enabled then
    fun fmt -> Printf.eprintf ("\027[35m"^^fmt^^"\027[m\n%!")
  else
    fun fmt -> Printf.ifprintf stderr fmt

let printexc =
  let pr e = match e with
    | Unix.Unix_error (err,fn,_param) ->
      Printf.sprintf "%s: %s" fn (Unix.error_message err)
    | e -> Printexc.to_string e
  in
  if debug_enabled then fun e ->
    Printf.sprintf "%s\n%s" (pr e) (Printexc.get_backtrace ())
  else fun e ->
    pr e

exception Recoverable_error of string
let recover_error fmt =
  Printf.ksprintf
    (fun s ->
      debug "Error: %s" s;
      raise (Recoverable_error s))
    fmt

let split_lines str =
  let len = String.length str in
  let rec get_nls i =
    if i >= len then []
    else if str.[i] = '\n' then i::get_nls (succ i)
    else get_nls (succ i)
  in
  let nls = get_nls 0 in
  let string_sub str start stop =
    let start =
      if start < stop && str.[start] = '\n' then start + 1 else start in
    let stop =
      if stop > start && str.[stop-1] = '\r' then stop - 1 else stop in
    String.sub str start (stop - start)
  in
  List.map2 (string_sub str) (0::nls) (nls@[len])

module File = struct

  let load : 'a. string -> (string -> 'a) -> 'a = fun filename k ->
    debug "Loading %s" filename;
    let contents =
      try
        let size = (Unix.stat filename).Unix.st_size in
        if size > Sys.max_string_length then
          failwith "Maximum file size exceeded";
        let ic = open_in_bin filename in
        let buf = really_input_string ic size in
        close_in ic;
        buf
      with
        Unix.Unix_error _ | Sys_error _ as e ->
          recover_error "<b>Error loading file <i>%s</i>:</b>\n%s"
            filename (printexc e)
    in
    contents |> k

  let save contents filename k =
    debug "Saving %S" filename;
    let () =
      try
        let oc = open_out_bin filename in
        output_string oc contents;
        close_out oc
      with
        Unix.Unix_error _ | Sys_error _ as e ->
          recover_error "<b>Error saving file <i>%s</i>:</b>\n%s"
            filename (printexc e)
    in
    () |> k
end
