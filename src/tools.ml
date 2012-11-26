module Ops = struct
  external (@@) : ('a -> 'b) -> 'a -> 'b = "%apply"
  external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"
end
include Ops

let debug_enabled =
  try Sys.getenv "OCP_DEBUG" <> "0" with Not_found -> false

let debug =
  if debug_enabled then
    fun fmt -> Printf.eprintf ("[34m"^^fmt^^"[m\n%!")
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

module File = struct

  let load filename k =
    debug "Attempting to load %s" filename;
    let contents =
      try
        let size = (Unix.stat filename).Unix.st_size in
        if size > Sys.max_string_length then
          failwith "Maximum file size exceeded";
        let ic = open_in filename in
        let buf = String.create size in
        really_input ic buf 0 size;
        close_in ic;
        debug "%s loaded" filename;
        buf
      with
        Unix.Unix_error _ | Sys_error _ as e ->
          recover_error "<b>Error loading file <i>%s</i>:</b>\n%s"
            filename (printexc e)
    in
    contents |> k

  let save contents filename k =
    debug "Attempting to save %S" filename;
    let () =
      try
        let oc = open_out filename in
        output_string oc contents;
        close_out oc;
        debug "saved to %s" filename
      with
        Unix.Unix_error _ | Sys_error _ as e ->
          recover_error "<b>Error saving file <i>%s</i>:</b>\n%s"
            filename (printexc e)
    in
    () |> k
end
