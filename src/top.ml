open Tools.Ops

type status =
  | Ready
  | Busy of string
  | Dead

type response =
  | Message of string
  (* Response message from ocaml. We get this from stdout *)
  | User of string
  (* User output from his ocaml program. Is passed through stderr *)
  | Exited
  (* The toplevel exited or was terminated *)

type t = {
  pid: int;
  query_channel: out_channel;
  response_channel: response Event.channel;
  (* error_channel ? *)
  mutable status_change_hook: t -> unit;
  mutable status: status;
  mutable receive_hook: (response -> unit) option;
  mutable exit_hook: unit -> unit;
}

let set_status t st = t.status <- st; t.status_change_hook t

exception Not_running

let main_thread = Thread.self ()

(* Returns an event that handles a single message from ocaml and passes
   the message as a string to a function *)
let receive_event t f =
  let evt = Event.receive t.response_channel in
  let evt = Event.wrap evt  @@ fun resp ->
      f resp;
      if resp = Exited && t.status <> Dead then (* we already know *)
        (Tools.debug "Toplevel exit event received";
         t.status <- Dead; (* don't run the hook yet *)
         t.exit_hook ();
         set_status t Dead);
      resp
  in
  let evt = Event.wrap evt @@ fun resp ->
      match t.receive_hook with
      | Some f -> f resp
      | None -> ()
  in
  evt

(* After some experiments, Glib IO through lablGtk didn't turn out well on
   Windows: we read from the ocaml process manually with a dedicated thread *)
let reader_thread fdescr receive_from_main_thread build_response t =
  let buf_len = 4096 in
  let buf = String.create buf_len in
  let rec loop () =
    if t.status = Dead then
      (Tools.debug "Ocaml process %d dead, reader thread terminating" t.pid;
       Thread.exit ());
    try
      let nread = Unix.read fdescr buf 0 buf_len in
      if nread <= 0 then
        (Tools.debug "Error reading from the ocaml process, \
                      terminating reader thread";
         Thread.exit ());
      let response = String.sub buf 0 nread in
      (* Tools.debug "Incoming response from ocaml: %d %s" nread response; *)
      if t.status = Dead then
        (Tools.debug "OCaml process marked as dead, terminating reader thread";
         Thread.exit ());
      receive_from_main_thread ();
      Event.sync (Event.send t.response_channel (build_response response));
      loop ()
    with e ->
        Tools.debug "ERROR IN READER THREAD: %s" (Printexc.to_string e)
  in
  loop ()

(* notifies if the ocaml process terminates *)
let watchdog_thread receive_from_main_thread t =
  let _ =
    try
      ignore @@ Unix.waitpid [] t.pid
    with Unix.Unix_error _ ->
        Tools.debug "Watchdog: waitpid returned an error"
  in
  Tools.debug "Watchdog wakes up: ocaml %d is dead" t.pid;
  receive_from_main_thread ();
  Event.sync (Event.send t.response_channel Exited);
  Tools.debug "Watchdog exits: death of ocaml has been signalled"


let start schedule response_handler status_hook =
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
    Unix.create_process_env "ocaml"
      [|"ocaml";"-nopromptcont";"-init";"toplevel_init.ml"|]
      env
      top_stdin top_stdout top_stderr
  in
  Tools.debug "Toplevel started";
  List.iter Unix.close [top_stdin; top_stdout; top_stderr];
  (* Build the top structure *)
  let t = {
    pid = ocaml_pid;
    query_channel = Unix.out_channel_of_descr query_fdescr;
    response_channel = Event.new_channel ();
    status_change_hook = (fun t -> status_hook t.status);
    status = Busy "init";
    receive_hook = None;
    exit_hook = (fun () -> ());
  } in
  let event_receive = receive_event t response_handler in
  let receive_from_main_thread () =
    schedule (fun () ->
      assert (Thread.self () = main_thread);
      Event.sync event_receive)
  in
  let _response_reader_thread =
    Thread.create
      (reader_thread response_fdescr
         receive_from_main_thread
         (fun resp -> Message resp)) t
  in
  let _error_reader_thread =
    Thread.create
      (reader_thread error_fdescr
         receive_from_main_thread
         (fun resp -> User resp)) t
  in
  let _watchdog_thread =
    Thread.create
      (watchdog_thread receive_from_main_thread) t
  in
  t.exit_hook <- (fun () ->
    List.iter Unix.close [query_fdescr; response_fdescr; error_fdescr];
    (* Tools.debug "Collecting threads..."; *)
    List.iter Thread.join
      [_response_reader_thread; _error_reader_thread; _error_reader_thread];
    (* Tools.debug " done" *)
  );
  set_status t Ready;
  t

let add_status_change_hook t f =
  let current = t.status_change_hook in
  t.status_change_hook <- fun t -> current t; f t.status

let flush t = flush t.query_channel

let await_full_response cont =
  let buf = Buffer.create 857 in
  let buffer_rm_suffix buf suf =
    let len = Buffer.length buf and suf_len = String.length suf in
    if len >= suf_len && Buffer.sub buf (len - suf_len) suf_len = suf
    then Some (Buffer.sub buf 0 (len - suf_len))
    else None
  in
  function
  | Message s ->
      Buffer.add_string buf s;
      (* fragile way to detect end of answer *)
      (match buffer_rm_suffix buf "# " with
      | Some s -> s |> cont
      | None -> ())
  | User _ -> ()
  | Exited ->
      Buffer.contents buf |> cont

let query t q cont =
  try
    assert (t.receive_hook = None);
    t.receive_hook <-
      Some (await_full_response @@ fun response ->
          t.receive_hook <- None;
          set_status t Ready;
          cont response);
    output_string t.query_channel q;
    output_string t.query_channel ";;\n";
    flush t;
    set_status t (Busy q)
  with Sys_error _ ->
      Tools.debug "Could not write to ocaml process %d" t.pid
        (* Death should be signalled by watchdog thread *)

(* No proper signals on Windows, we have to use a stub to send a signal to the
   console *)
external sigint : int -> unit = "send_sigint"
let stop t =
  match t.status with
  | Busy _ -> sigint t.pid
  | Ready | Dead -> ()

external terminate : int -> unit = "terminate"
let kill t =
  match t.status with
  | Dead ->
      Tools.debug
        "Not killing toplevel %d: according to the records, it's already dead."
        t.pid
  | Ready | Busy _ ->
      terminate t.pid
