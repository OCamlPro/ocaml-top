let file_contents filename =
  let size = (Unix.stat filename).Unix.st_size in
  if size > Sys.max_string_length then failwith "Maximum file size exceeded";
  let ic = open_in filename in
  let buf = String.create size in
  really_input ic buf 0 size;
  close_in ic;
  buf

let save_to_file str filename =
  let oc = open_out filename in
  output_string oc str;
  close_out oc

