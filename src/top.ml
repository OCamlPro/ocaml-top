(* We want non-blocking input and are stuck with the scheduler from Gtk (aka
   main loop) here. So we can either go through Glib's IO, or spawn a thread for
   polling. The former sounds simpler and safer, although lablGTK is lacking in
   this area... *)
module GIO = Glib.Io

open Tools.Ops

type status =
| Ready
| Busy of string

type t = {
  pid: int;
  query_channel: out_channel;
  response_channel: string Event.channel;
  (* error_channel: GIO.channel; *)
  mutable status: status;
}


let start () =
  let top_stdin,query_fdescr = Unix.pipe() in
  let response_fdescr,top_stdout = Unix.pipe() in
  let error_fdescr,top_stderr = Unix.pipe() in
  let env = (* filter TERM out of the environment *)
    Unix.environment ()
    |> Array.fold_left
        (fun acc x ->
          if String.length x >= 5 && String.sub x 0 5 = "TERM=" then acc
          else x::acc) []
    |> List.rev
    |> Array.of_list
  in
  let ocaml_pid =
    Unix.create_process_env "ocaml" [|"ocaml";"-nopromptcont"|] env
      top_stdin top_stdout top_stderr
  in
  Tools.debug "Toplevel started";
  (* now set up reading thread *)
  let callback evt_channel =
    let buf_len = 4096 in
    let buf = String.create buf_len in
    let rec loop () =
      let nread = Unix.read response_fdescr buf 0 buf_len in
      (* GMain.Event.propagate: signal we got some input *)
      let evt = Event.send evt_channel (String.sub buf 0 nread) in
      Tools.debug "Incoming response from ocaml";
      Event.sync evt;
      loop ()
    in
    loop ()
  in
  let evt_channel: string Event.channel = Event.new_channel () in
  let _reading_thread = Thread.create callback evt_channel in
  (* Build the top structure *)
  let t = {
    pid = ocaml_pid;
    query_channel = Unix.out_channel_of_descr query_fdescr;
    response_channel = evt_channel;
    (* error_channel = GIO.channel_of_descr error_fdescr; *)
    status = Busy "init";
  } in
  t

let filter_top_output t str =
  let len = String.length str in
  if len >= 1 && String.sub str (len - 2) 2 = "# " then
    (t.status <- Ready;
     String.sub str 0 (len - 2))
  else
    str

let watch t f =
  (* delayed watch to avoid triggering the callback repeatedly for single words
     if ocaml spams its output *)
  let callback () =
    match Event.poll (Event.receive t.response_channel) with
    | None -> true
    | Some s ->
      f @@ filter_top_output t s; true
  in
  ignore @@ Glib.Timeout.add ~ms:50 ~callback

let flush t = flush t.query_channel

let query t q =
  let q = String.trim q in
  let len = String.length q in
  output_string t.query_channel q;
  if len >= 2 && String.sub q (len-2) 2 = ";;" then
    output_string t.query_channel "\n"
  else
    output_string t.query_channel ";;\n";
  flush t;
  t.status <- Busy q

(* Doesn't work. And no signals in Windows...
   This is likely to need quite a bit of work. Some references:
   - https://sympa.inria.fr/sympa/arc/caml-list/2005-06/msg00261.html (but the link to the source is broken !)
   - http://stackoverflow.com/questions/813086/can-i-send-a-ctrl-c-sigint-to-an-application-on-windows (this really looks overly complicated, needing an intermediate process just intended to kill itself with the target ?)
   this stub from stackoverflow might work:

   void SendControlC(int pid)
   {
   AttachConsole(pid); // attach to process console
   SetConsoleCtrlHandler(NULL, TRUE); // disable Control+C handling for our app
   GenerateConsoleCtrlEvent(CTRL_C_EVENT, 0); // generate Control+C event
   }
*)
let stop t =
  flush t;
  (* FIXME FOR WINDOWS *)
  Unix.kill t.pid 2 (* SIGINT *)
  (* we'd like to flush the output of ocaml... but I couldn't find a way to do
     that in lablgtk+glib *)
