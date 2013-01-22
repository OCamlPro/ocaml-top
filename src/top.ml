open Tools.Ops

type status =
| Ready
| Busy of string

type t = {
  pid: int;
  query_channel: out_channel;
  response_channel: string Event.channel;
  (* error_channel ? *)
  mutable status: status;
}

let filter_top_output t str =
  let len = String.length str in
  if len >= 1 && String.sub str (len - 2) 2 = "# " then
    (t.status <- Ready;
     String.sub str 0 (len - 2))
  else
    str

(* After some experiments, Glib IO through lablGtk didn't turn out well on
   Windows: we read from the ocaml process manually with a dedicated thread *)
let start schedule response_handler =
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
  let main_thread = Thread.self () in
  let callback t =
    let evt_send s = Event.send t.response_channel s
    and evt_receive =
      Event.wrap (Event.receive t.response_channel) (filter_top_output t)
    in
    let buf_len = 4096 in
    let buf = String.create buf_len in
    let rec loop () =
      let nread = Unix.read response_fdescr buf 0 buf_len in
      if nread > 0 then
        let evt = evt_send (String.sub buf 0 nread) in
        Tools.debug "Incoming response from ocaml";
        schedule (fun () ->
          assert (Thread.self () = main_thread);
          (* The schedule function must push this code back to the main thread *)
          Event.sync evt_receive |> response_handler);
        Event.sync evt;
        loop ()
      else (* todo: handle ocaml termination *)
        (Tools.debug "Error reading from the ocaml process";
         loop())
    in
    loop ()
  in
  (* Build the top structure *)
  let t = {
    pid = ocaml_pid;
    query_channel = Unix.out_channel_of_descr query_fdescr;
    response_channel = Event.new_channel ();
    status = Busy "init";
  } in
  let _reading_thread = Thread.create callback t in
  t

let response t =
  Event.sync (Event.receive t.response_channel)
  |> filter_top_output t

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
external sigint : int -> unit = "send_sigint"

let stop t =
  flush t;
  sigint t.pid
