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

module Ops = struct
  external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
end
include Ops

let debug_enabled =
  try Sys.getenv "OCP_DEBUG" <> "0" with Not_found -> false

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

let string_split_chars chars str =
  let len = String.length str in
  let rec split pos =
    let rec lookup i =
      if i >= len then raise Not_found
      else if String.contains chars str.[i] then i
      else lookup (succ i)
    in
    try
      let i = lookup pos in
      if i > pos then String.sub str pos (i - pos) :: split (succ i)
      else split (succ i)
    with Not_found | Invalid_argument _ ->
        if pos < len then [ String.sub str pos (len - pos) ]
        else []
  in
  split 0

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
        let buf = String.create size in
        really_input ic buf 0 size;
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
