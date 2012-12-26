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
  response_channel: GIO.channel;
  error_channel: GIO.channel;
  mutable status: status;
}


let start () =
  let top_stdin,query_fdescr = Unix.pipe() in
  let response_fdescr,top_stdout = Unix.pipe() in
  let error_fdescr,top_stderr = Unix.pipe() in
  let ocaml_pid =
    Unix.create_process_env "ocaml" [|"ocaml";"-nopromptcont"|]
      [| "PATH="^Sys.getenv "PATH"|] (* unset the TERM variable *)
      top_stdin top_stdout top_stderr
  in
  Tools.debug "Toplevel started";
  Unix.set_nonblock response_fdescr;
  let t = {
    pid = ocaml_pid;
    query_channel = Unix.out_channel_of_descr query_fdescr;
    response_channel = GIO.channel_of_descr response_fdescr;
    error_channel = GIO.channel_of_descr error_fdescr;
    status = Ready;
  } in
  t

(* lablgtk's interface for Glib reading is weird: different and more limited
   than the original Glib one. This function just reads what it can from the
   channel and returns it as a string. *)
let gread =
  let len = 4096 in
  let buf = Buffer.create len in
  let str = String.create len in
  fun ch ->
    let rec try_read () =
      try
        let n = GIO.read_chars ch ~buf:str ~pos:0 ~len in
        (* Optim-todo: we may skip the buffer if only one read was enough (most
           of the time) *)
        if n > 0 then
          (Buffer.add_substring buf str 0 n; try_read())
      with
      | Unix.Unix_error (Unix.EAGAIN,_,_)
      | Unix.Unix_error (Unix.EWOULDBLOCK,_,_) ->
        ()
      (* Why, WHY do they wrap this in a string ?? *)
      | Glib.GError s as exc ->
        let i = String.index s ' ' in
        let e = String.sub s (i+1) (String.length s - i - 1) in
        match e with
        | "G_IO_STATUS_AGAIN" -> ()
        | _ ->
          Tools.debug "Glib io error in gread: «%s»" s;
          raise exc
    in
    try_read ();
    (* todo: sanitize the contents (if the user is allowed i/o from the ocaml
       program): may be invalid utf-8 *)
    let ret = Buffer.contents buf in
    Buffer.clear buf;
    ret

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
  let rec callback1 _ =
    ignore @@ Glib.Timeout.add ~ms:50 ~callback:callback2;
    false
  and callback2 _ =
    f @@ filter_top_output t @@ gread t.response_channel;
    delayed_watch ();
    false
  and delayed_watch () =
    ignore @@
      GIO.add_watch t.response_channel ~cond:[ `IN ] ~prio:20 ~callback:callback1;
  in
  delayed_watch ()

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

let response t =
  gread t.response_channel

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
