
type t = {
  query_channel: out_channel;
  response_channel: in_channel;
}


let start () =
  let response_channel,query_channel =
    Unix.open_process "ocaml"
  in
  { query_channel; response_channel; }

let flush t = flush t.query_channel

let query t q =
  output_string t.query_channel q;
  output_string t.query_channel ";;\n";
  flush t

let response t =
  input_line t.response_channel

let stop t =
  output_char t.query_channel '';
  flush t

